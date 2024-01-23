! add limited number of shots, regain ammo when an enemy's ship is sunk, and lose ammo when one of your ships is sunk, equal to the length of that ship multiplied by a scale factor
! replace symbols for blank space, miss and hit: "-" -> " ", "M" -> ".", "H"-> "*"

PROGRAM battleships_game

use iso_fortran_env, only : iostat_end, stdout=>output_unit, stdin=>input_unit
USE battleships_mod
use misc_mod, only: sleep, strip_spaces, convert_char2num, convert_charnum2num

IMPLICIT NONE

INTEGER :: ans_n1, ans_n2, e1, e2, c, i, r, ax, ay1, ay2, hit_ship = 0, ios, u
CHARACTER :: ans*3, ans_char*1, char_in*32, char_one*1, char_temp*32, tag_name*32, tag_value*32, msg*64 = ''
CHARACTER (LEN=2), DIMENSION(1:10,1:10) :: defence, comp, offence*1
LOGICAL :: sunk, two_ai = .FALSE., configExist
LOGICAL, DIMENSION(1:5) :: p1sunk, p2sunk

TYPE(ship), DIMENSION(1:5) :: player1, player2
TYPE(target) :: p1t
TYPE(scoreboard_entity), DIMENSION(1:2) :: hits
TYPE(scoreboard_entity), DIMENSION(1:5) :: health
CHARACTER (LEN=32) :: hits_title, health_title
TYPE(ai_saved_data) :: ai1, ai2

CHARACTER :: config_location*32 = 'config.dat', &
            & seed_operation*4 = 'save', &
            & seed_bank*4 = '0'

! ========
!  CONFIG
! ========
LOGICAL :: delay = .TRUE., GET_TO_THE_END = .FALSE.
character, parameter :: ESC = achar(27)
CHARACTER(4), parameter :: CLEAR_SCREEN = ESC // "[2J" !< erase screen and move to top left

WRITE(stdout,'(a)',ADVANCE='no') 'Looking for "'//TRIM(config_location)//'" ... '
INQUIRE(FILE=config_location, EXIST=configExist)

IF(configExist) THEN ! file already exists, open with old status

  print '(a)', 'Success, loading configuration'
  OPEN(newunit=u, FILE=config_location, STATUS='old')

  DO
!      WRITE(stdout,'(a)',ADVANCE='no') 'Searching for headers ... '
    READ(u,'(a)',IOSTAT=ios) char_in
    IF(ios /= 0) exit
    !char_in = ADJUSTL(char_in) ! contract to remove spaces?
    CALL strip_spaces( char_in )
!      print '(a)', 'line="'//TRIM(char_in)//'"'

    IF(char_in(1:1) == '[') THEN ! type found
      i = LEN(TRIM(char_in))
      READ(char_in(i:i),'(a1)') char_one
      !print '(a,i0,a)', 'LEN=',i,' last="'//char_one//'"'

      IF(char_one == ']') THEN
        READ(char_in(2:i-1),'(a)') char_temp
!          print '(a)', 'Found header ['//TRIM(char_temp)//']'

        SELECT CASE(char_temp)
          CASE('seed')
            DO ! search for tags
!                WRITE(stdout,'(a)',ADVANCE='no') ' Searching for tags ... '
              READ(u,'(a)',IOSTAT=ios) char_in
              IF(ios /= 0) exit
              ! search for equals sign
              !print '(a)', '  tagLine="'//TRIM(char_in)//'"'
              i = 1
              DO
                READ(char_in(i:i),'(a1)') char_one
                !print '(a,i0)', 'i=',i
                IF(char_one == '=') EXIT
                IF(i == LEN(TRIM(char_in))) error STOP 'error: could not identify a tag'
                i = i + 1
              END DO
              !print '(a,i0)', ' Found = at i=',i
              READ(char_in(1:i-1),'(a)') tag_name
!                print '(a)', '  tagName="'//TRIM(tag_name)//'"'

              READ(char_in(i+1:LEN(TRIM(char_in))),'(a)') tag_value
!                print '(a)', '  tagValue="'//TRIM(tag_value)//'"'

              SELECT CASE(tag_name)
                CASE('operation')
                  seed_operation = TRIM(tag_value)

                CASE('bank')
                  seed_bank = TRIM(tag_value)

              END SELECT

              !print '(a)', 'Found "'//char_temp//'"="'//seed_

            END DO

          CASE DEFAULT
            error STOP 'error: unknown config type'
        END SELECT

      ELSE
        error STOP 'error: could not find type'
      END IF
      !i = 1
      !DO
      !  READ(char_in(i:LEN(TRIM(char_in))),'(a1)',IOSTAT=ios) char_one; IF(ios /= 0) STOP 'error: bad read'
      !  print '(a,i0,a)', 'i=',i,' char_one='//char_one
      !  IF(char_one == ']') EXIT
      !  i = i + 1
      !END DO
      !print '(a,i0)', 'i=',i
    END IF
  END DO
ELSE ! file does not already exist, so make a new one with default values
  print '(a)', 'Failed, creating default configuration'
  OPEN(newunit=u, FILE=config_location, STATUS='new')
  WRITE(u,'(a)') '[seed]'
  WRITE(u,'(a)') 'operation='//TRIM(seed_operation)
  WRITE(u,'(a)') 'bank='//TRIM(seed_bank)
END IF
CLOSE(u)

! set seed for RNG
CALL random_init(.false., .false.)

call sleep(1000)
!WRITE(stdout,'(a)',ADVANCE='no') 'Press enter to continue...'; read(stdin,*)
!  STOP 'debug'

! initialise grid, error values, and sunk variables
defence = CB//CB; offence = CB; comp = CB//CB
e1 = 0; e2 = 0
p1sunk = .FALSE.; p2sunk = .FALSE.

! set up hits scoreboard
hits_title = 'Score'
hits(1)%n = 'Player 1'; hits(1)%s = 0
hits(2)%n = 'Player 2'; hits(2)%s = 0
hits(1:2)%t = 5

! set up symbols, length and initial state
player1(1)%nu = 'D'; player1(1)%nl = 'd'; player1(1)%l = 2
player1(2)%nu = 'C'; player1(2)%nl = 'c'; player1(2)%l = 3
player1(3)%nu = 'S'; player1(3)%nl = 's'; player1(3)%l = 3
player1(4)%nu = 'B'; player1(4)%nl = 'b'; player1(4)%l = 4
player1(5)%nu = 'A'; player1(5)%nl = 'a'; player1(5)%l = 5
player1(1:5)%s = CB; player1(1:5)%x = 0; player1(1:5)%y = 0; player1(1:5)%z = CN; player1(1:5)%h = 0

player2(1)%nu = 'D'; player2(1)%nl = 'd'; player2(1)%l = 2
player2(2)%nu = 'C'; player2(2)%nl = 'c'; player2(2)%l = 3
player2(3)%nu = 'S'; player2(3)%nl = 's'; player2(3)%l = 3
player2(4)%nu = 'B'; player2(4)%nl = 'b'; player2(4)%l = 4
player2(5)%nu = 'A'; player2(5)%nl = 'a'; player2(5)%l = 5
player2(1:5)%s = CB; player2(1:5)%x = 0; player2(1:5)%y = 0; player2(1:5)%z = CN; player2(1:5)%h = 0

! set up names
player1(1)%n = 'Destroyer'        ; player2(1)%n = 'Destroyer'
player1(2)%n = 'Cruiser'          ; player2(2)%n = 'Cruiser'
player1(3)%n = 'Submarine'        ; player2(3)%n = 'Submarine'
player1(4)%n = 'Battleship'       ; player2(4)%n = 'Battleship'
player1(5)%n = 'Aircraft Carrier' ; player2(5)%n = 'Aircraft Carrier'

! set up ships' health scoreboard
health_title = 'Health'
DO i=1,5
  health(i)%n = player1(i)%nu
  health(i)%s = player1(i)%l
  health(i)%t = player1(i)%l
END DO

main_menu : do

  write(stdout,"(a)", advance="no") CLEAR_SCREEN
  CALL logo
  print '(/,a)', msg
  print '(/,a)', ' Main Menu'
  print '(a)', ' # Option'
  print '(a)', ' 1 Place your ships'
  print '(a)', ' 2 Begin battle!'
  print '(a)', ' 3 Watch an automated battle'
  print '(a)', ' 4 About'
  DO
    WRITE(stdout,'(a,1x)', ADVANCE='no') '<I> Enter your chosen option from the # column [1-4]:'
    read(stdin,'(i1)', iostat=i) ans_n1
    if(i == iostat_end) stop
    if(i /= 0) cycle
    IF(ans_n1 >= 1 .AND. ans_n1 <= 4) EXIT
  END DO

  SELECT CASE(ans_n1)
    CASE(1)
      DO ! Display ship placement menu
        write(stdout,"(a)", advance="no") CLEAR_SCREEN

        CALL grid_1(defence,'d')
        print '(/,a)',                  ' Ship Placement Menu'
        print '(a)',                  ' #       Option         Symbols Length'
        print '(a)',                  ' 0 Return to main menu'
        print '(a)',                  ' 1 Help'
        print '(a,2x,2(a,1x),4x,i1)', ' 2 Destroyer          ', player1(1)%nu, player1(1)%nl, player1(1)%l
        print '(a,2x,2(a,1x),4x,i1)', ' 3 Cruiser            ', player1(2)%nu, player1(2)%nl, player1(2)%l
        print '(a,2x,2(a,1x),4x,i1)', ' 4 Submarine          ', player1(3)%nu, player1(3)%nl, player1(3)%l
        print '(a,2x,2(a,1x),4x,i1)', ' 5 Battleship         ', player1(4)%nu, player1(4)%nl, player1(4)%l
        print '(a,2x,2(a,1x),4x,i1)', ' 6 Aircraft Carrier   ', player1(5)%nu, player1(5)%nl, player1(5)%l
        print '(a)',                  ' 7 Auto assign'
        DO
          write(stdout,'(a)', ADVANCE='no') '<I> Enter your chosen option from the # column [0-7]: '
          read(stdin,'(i1)', iostat=i) ans_n2
          if(i == iostat_end) stop
          if(i /= 0) cycle
          IF(ans_n2 >= 0 .AND. ans_n2 <= 7) EXIT
        END DO

        SELECT CASE(ans_n2)
          CASE (0)
            EXIT
          CASE (1)
            print '(/,a,/)', ' How to assign your ships to the DEFENCE grid'
            print '(a)', ' Ships are defined by the x and y coordinates of their bow (front),'
            print '(a)', ' and the direction (z) they face if they were to move "forwards"'
            print '(a)', ' (the ships do not actually move in the game).'
            print '(/,a)', ' e.g. (x,y,z) = (A,1,N) would place a ship facing north with its'
            print '(a)', ' bow at A1, and the rest of the ship trailing south, like this:'
            print '(/,a)', '                            A  B  '
            print '(a)', '                          1 D- -- '
            print '(a)', '                          2 d- -- '
            print '(a)', '                          3 -- -- '
            print '(/,a)', ' The x and y coordinates should be entered together when prompted.'
            print '(a)', ' e.g. E5'
            write(stdout,'(/,a)',ADVANCE='no') 'Press enter to continue...'; read(stdin,*)

        CASE (2:6) ! set ship data and write to defence array
          IF(e2 == 0) THEN
            CALL clearship( player1(ans_n2-1), defence )
          END IF
          CALL shipdata( player1(ans_n2-1), defence, e1, e2 )
          IF(e1 == 0 .AND. e2 == 0) THEN
            CALL setship( player1(ans_n2-1), defence)
          END IF

        CASE (7)
          DO
            print '(a)', '<I> Do you want to clear the current ship arrangement'
            write(stdout,'(a,1x)',ADVANCE='no')' and assign random ships automagically? [Y/N]:'
            read(stdin,'(a1)', iostat=i) ans_char
            if(i == iostat_end) stop
            if(i /= 0) cycle
            IF(ans_char == 'y' .OR. ans_char == 'n' .or. ans_char == 'Y' .OR. ans_char == 'N') EXIT
          END DO
          IF(ans_char == 'y' .OR. ans_char == 'Y') THEN
            defence = CB//CB
            CALL ranship(player1,defence)
            e1 = 0; e2 = 0
          END IF

        END SELECT

      END DO

    CASE (2) ! Check for errors in ship placement before beginning battle
      ! Check if ships have been placed
      c = 0
      DO i=1,5
        IF(player1(i)%z == CN) THEN
          c = c + 1
        END IF
      END DO

      !c = 0; e1 = 0; e2 = 0 ! uncomment for debugging
      IF(e1 == 0 .AND. e2 == 0 .AND. c == 0) EXIT

      IF(e1 /= 0 .AND. e2 /=0) THEN
        print '(a)', ' There are unresolved errors in the ship placement.'
      END IF

      IF(c /= 0) THEN
        msg = ' You need to place all 5 ships before going into battle!'
      END IF

    CASE (3) ! automated battle

      DO
        print '(a)', '<I> This will clear the current ship arrangement'
        write(stdout,'(a,1x)',ADVANCE='no')' do you wish to continue? [Y/N]:'
        read(stdin,'(a1)', iostat=i) ans_char
        if(i == iostat_end) stop
        if(i /= 0) cycle
        IF(ans_char == 'y' .OR. ans_char == 'n' .or. ans_char == 'Y' .OR. ans_char == 'N') EXIT
      END DO
      IF(ans_char == 'y' .OR. ans_char == 'Y') THEN
        two_ai = .TRUE.
        e1 = 0; e2 = 0 ! error checking variables for player ship placement are irrelevant now
        FORALL (i=1:5) player1(i)%z = ACHAR( IACHAR(CN) + 1 )  ! set to dummy char that is not CN
        msg = ' Ready to play automated battle, for your spectating pleasure!'
      END IF

    CASE (4)
      print '(a)', ' This program was made by psq95,'
      print '(a)', ' written in Fortran 90 at the University of Surrey, 2013.'
      print '(a)', ' Latest build: Dec 2017.'

      print '(/,a)', ' References'
      print '(a)', '  1. ASCII font ''cybermedium''&
      & http://www.topster.de/text-to-ascii/cybermedium.html '
      !print '(a)', '  2. ASCII target/crosshair '

      call sleep(2000)

  END SELECT
END DO main_menu

IF(two_ai) THEN
  CALL ranship(player1,defence) ! offence
END IF

! debug loop for player2's ships
!DO
  CALL ranship(player2,comp)
  !CALL grid_1(comp,'c')
  !comp = '--'
  !CALL SLEEP(1000)
!END DO

! do battle
r = 1 ! round counter

write(stdout,"(a)", advance="no") CLEAR_SCREEN
write(stdout,*)
CALL grid_2(defence,offence)
CALL scoreboard(hits_title,hits)
CALL scoreboard(health_title,health)

DO ! round loop

  ! Player 1's turn

  print '(/,a,i2,/)', 'Round ',r
  print '(a)', "Player 1's turn."

  ! To enable automated battle, need to move player interaction to subroutine

  IF(two_ai) THEN

    CALL ai(comp,player2,p2sunk,hits,1,delay,ai1,offence=offence)

  ELSE

    DO ! check loop

      DO ! input loop
        write(stdout,'(a,1x)', ADVANCE='no') '<I> Enter target x- and y- coordinates, e.g. A2 or A10 [A-J 1-10]:'
        read(stdin,'(a3)', iostat=i) ans ! e.g. ans = A1 to A10, B1 to B10 etc.
        if(i == iostat_end) stop
        if(i /= 0) cycle
        c = 0; ax = IACHAR(ans(1:1)); ay1 = IACHAR(ans(2:2)); ay2 = IACHAR(ans(3:3))
        IF(ax >= 65 .AND. ax <= 74 .OR. ax >= 97 .AND. ax <= 106) c = c + 1 ! 65-74 = 'A'-'J', 97-106 = 'a'-'j'
        IF(ay1 >= 49 .AND. ay1 <= 57) c = c + 1 ! 49 = '1', 57 = '9'
        IF(ay2 == 32 .OR. (ay2 == 48 .AND. ay1 == 49)) c = c + 1 ! 32 = ' ' blank, 48 = '0'
        !write(stdout,'(2a)') 'ans(1:1) = ',ans(1:1)
        !write(stdout,'(2a)') 'ans(2:2) = ',ans(2:2)
        !write(stdout,'(2a)') 'ans(3:3) = ',ans(3:3)
        IF(c == 3) EXIT
        !print '(a)', '<ERROR> Try again.'
      END DO ! input loop

      CALL convert_char2num(ans(1:1),p1t%x)
      CALL convert_charnum2num(ans(2:3),p1t%y)
      CALL display_target(p1t%x,p1t%y)

      p1t%s = comp(p1t%x,p1t%y) ! query comp at point for complete state
      IF(p1t%s(2:2) == CB) EXIT

      print '(a)', "You've already fired there!"
      print '(a)', 'Try again.'

    END DO ! check loop

    IF( delay ) CALL sleep(500)
    IF( p1t%s(1:1) == CB ) THEN

      print '(a)', 'Miss!'
      offence(p1t%x,p1t%y) = CM ! record result in OFFENCE
      comp(p1t%x,p1t%y)(2:2) = CM

    ELSE

      print '(a)', 'Hit!'
      offence(p1t%x,p1t%y) = CH ! record result in OFFENCE
      comp(p1t%x,p1t%y)(2:2) = CH
      ! Identify which ship was hit and determine if it has been sunk.
      CALL identify( p1t, player2, hit_ship, sunk )
      ! hit_ship = [1,2,3,4,5], sunk = [.TRUE./.FALSE.]
      ! create two arrays of logicals to record how many of each player's ships are sunk.
      p2sunk(hit_ship) = sunk
      IF( sunk ) THEN
        IF( delay ) CALL sleep(500)
        print '(3a)', "You've sunk my ",TRIM( player2( hit_ship )%n ),"!"
        hits(1)%s = hits(1)%s + 1
      END IF

    END IF

  END IF

  !p2sunk = .TRUE.

  IF( ALL( p2sunk ) ) THEN
    IF(delay) CALL sleep(500)
    print '(a)', "You've sunk all my ships!"
  END IF

  IF( delay ) CALL sleep(500)

  ! Player 2's turn

  print '(/,a)', "Player 2's turn."

  CALL ai( defence, player1, p1sunk, hits, 2, delay, ai2, health )

  !p1sunk = .TRUE.

  IF(ALL(p1sunk)) THEN
    IF(delay) CALL sleep(500)
    print '(a)', "I've sunk all your ships!"
  END IF

  IF(.NOT.GET_TO_THE_END) CALL sleep(500)
  write(stdout,"(a)", advance="no") CLEAR_SCREEN
  write(stdout,*)
  !CALL grid_1(comp,'c'); write(stdout,*)
  CALL grid_2(defence,offence)
  CALL scoreboard(hits_title,hits)
  CALL scoreboard(health_title,health)

  IF(ALL(p2sunk) .AND. ALL(p1sunk)) THEN

    IF(delay) CALL sleep(500)
    print '(a)', "It's a draw!"

  ELSE

    IF(ALL(p2sunk)) THEN
      IF(delay) CALL sleep(500)
      print '(a)', 'You win!'
    END IF

    IF(ALL(p1sunk)) THEN
      IF(delay) CALL sleep(500)
      print '(a)', 'You lose!'
    END IF

  END IF

  ! Determine if game ends. (all of a player's ships are sunk)
  IF(ALL(p2sunk) .OR. ALL(p1sunk)) EXIT ! exit round loop

  r = r + 1

END DO

IF(delay) CALL sleep(500)
print '(a)', 'Thanks for playing!'

END PROGRAM
