!=======================================================================
! psq95, Univeristy of Surrey, 2013
! Latest build: Dec 2017
!
! References
! 1. ASCII font 'cybermedium' http://www.topster.de/text-to-ascii/cybermedium.html
!========================================================================

MODULE battleships_mod

use iso_fortran_env, only : iostat_end, stdout=>output_unit

USE misc_mod

implicit none

CHARACTER(LEN=1), PARAMETER :: CH = '*', CM = '.', CB = ' ', CN = '0', GSV = '|', GSH = '=', GC = '+'

TYPE ship ! Destroyer, Cruiser, Submarine, Battleship, Aircraft Carrier
INTEGER :: x, y, l, h
! x = x-coordinate of bow, y = y-coordinate of bow, l = length of ship, h = number of hits
CHARACTER (LEN=1) :: z, nu, nl, s
! z = direction of bow [N,E,S,W]
! nu = name upper case [D,C,S,B,A] ! to identify the bow of the ship
! nl = name lower case [d,c,s,b,a] ! for the rest of ship
! s = state [-,S] ! [ok,sunk]
CHARACTER (LEN=16) :: n
END TYPE ship

TYPE target
INTEGER :: x,y
CHARACTER (LEN=2) :: s
END TYPE target

TYPE scoreboard_entity
CHARACTER (LEN=16) :: n ! name
INTEGER :: s, t ! score, total
END TYPE scoreboard_entity

TYPE ai_saved_data
INTEGER :: stage, pass, scan_stage, start, clock, pos, line_pos
INTEGER, DIMENSION(1:2) :: centre, try
INTEGER, DIMENSION(1:4,1:2) :: adjacent
INTEGER, DIMENSION(-4:4,1:2) :: line
LOGICAL :: first_time  = .TRUE.
END TYPE ai_saved_data

CONTAINS

SUBROUTINE scoreboard(title,entities)

CHARACTER (LEN=*), INTENT(IN) :: title
TYPE(scoreboard_entity), DIMENSION(:), INTENT(IN) :: entities
INTEGER :: i


print '(/,3x,a)', TRIM(title)

DO i=1,SIZE(entities)
  WRITE(stdout,"(4x,2a,i0,a,i0)") TRIM(entities(i)%n),' : ',entities(i)%s,' / ',entities(i)%t
END DO

END SUBROUTINE scoreboard

SUBROUTINE logo


print '(a)', "                       |                           "
print '(a)', "                       |                           "
print '(a)', "                 .  -  +  -  .                     "
print '(a)', "              +'       |       '+                  "
print '(a)', "            +          |          +                "
print '(a)', " ___  ____+___ ___ _   |____ ____ _ +_ _ ___  ____ "
print '(a)', " |__] |__|  |   |  |   ||___ [__  |__| | |__] [__  "
print '(a)', " |__] | /|  |   |  |___||___ ___] |  |\| |    ___] "
print '(a)', "       '               |               '           "
print '(a)', "       |               |               |           "
print '(a)', "   ----+---------------+---------------+----       "
print '(a)', "       |               |               |           "
print '(a)', "    _ _. _  ____ ____ ____ ___ ____ ____ _  _      "
print '(a)', "    | |\ |  |___ |  | |__/  |  |__/ |__| |\ |      "
print '(a)', "    | | \|  |    |__| |  \  |  |  \ |  | | \|      "
print '(a)', "          +            |            +              "
print '(a)', "            +          |          +                "
print '(a)', "              +.       |       .+                  "
print '(a)', "                 '  -  +  -  '                     "
print '(a)', "                       |                           "
print '(a)', "                       |                           "

END SUBROUTINE logo

SUBROUTINE grid_2(a1,a2)

CHARACTER (LEN=*), DIMENSION(:,:), INTENT(IN) :: a1, a2
INTEGER :: i, j
LOGICAL :: key

!                DEFENCE                    ^               OFFENCE
!    ++ A  B  C  D  E  F  G  H  I  J  ++    N       A B C D E F G H I J +
!     1 -- -- -- -- -- -- -- -- -- -- || < W E >  1 - - - - - - - - - - |
!     2 -- -- -- -- -- -- -- -- -- -- ||    S     2 - - - - - - - - - - |
!     3 -- -- -- -- -- -- -- -- -- -- ||    v     3 - - - - - - - - - - |
!     4 -- -- -- -- -- -- -- -- -- -- ||          4 - - - - - - - - - - |
!     5 -- -- -- -- -- -- -- -- -- -- ||          5 - - - - - - - - - - |
!     6 -- -- -- -- -- -- -- -- -- -- ||          6 - - - - - - - - - - |
!     7 -- -- -- -- -- -- -- -- -- -- ||          7 - - - - - - - - - - |
!     8 -- -- -- -- -- -- -- -- -- -- ||          8 - - - - - - - - - - |
!     9 -- -- -- -- -- -- -- -- -- -- ||          9 - - - - - - - - - - |
!    10 -- -- -- -- -- -- -- -- -- -- ||         10 - - - - - - - - - - |
!    ++ == == == == == == == == == == ++          + = = = = = = = = = = +


print '(15x,a,20x,a,13x,a)', 'DEFENCE','^','OFFENCE'

print '(6x,11(a,2x),2x,a,7x,11(a,1x))', &
  &'A','B','C','D','E','F','G','H','I','J',GC//GC,'N','A','B','C','D','E','F','G','H','I','J','+'

key = .TRUE.

DO j=1,10

  write(stdout,'((3x,i2,1x))', ADVANCE='no') j

  DO i=1,10
    write(stdout,'(a,1x)', ADVANCE='no') a1(i,j)
  END DO

  write(stdout,'(a)',ADVANCE='no') GSV//GSV

  IF(key) THEN

    SELECT CASE (j)
      CASE (1)
        write(stdout,'(4(1x,a))', ADVANCE='no') '<','W','E','>'
        write(stdout,'((1x,i2,1x))', ADVANCE='no') j
      CASE (2)
        write(stdout,'(4x,a)', ADVANCE='no') 'S'
        write(stdout,'((4x,i2,1x))', ADVANCE='no') j
      CASE (3)
        write(stdout,'(4x,a)', ADVANCE='no') 'v'
        write(stdout,'((4x,i2,1x))', ADVANCE='no') j
        key = .FALSE.
    END SELECT

  ELSE IF(.NOT. key) THEN

    write(stdout,'((9x,i2,1x))', ADVANCE='no') j

  END IF

  DO i=1,10
    write(stdout,'(a,1x)', ADVANCE='no') a2(i,j)
  END DO

  write(stdout,'(a)',ADVANCE='no') GSV

  write(stdout,*)

END DO

write(stdout,'(3x,a,1x)',ADVANCE='no') GC//GC
DO i=1,10
  write(stdout,'(a,1x)',ADVANCE='no') GSH//GSH
END DO
write(stdout,'(a)',ADVANCE='no') GC//GC

write(stdout,'(9x,a,1x)',ADVANCE='no') GC//GC
DO i=1,10
  write(stdout,'(a,1x)',ADVANCE='no') GSH
END DO
write(stdout,'(a)',ADVANCE='no') GC

write(stdout,*)

END SUBROUTINE grid_2

SUBROUTINE grid_1(a1,a)

CHARACTER (LEN=*), DIMENSION(:,:), INTENT(IN) :: a1
INTEGER :: i, j
CHARACTER(LEN=1) :: a
LOGICAL :: key

key = .FALSE.

SELECT CASE(a)

  CASE ('d')
    write(stdout,'(15x,a,20x,a)') 'DEFENCE','^'
    key = .TRUE. ! add compass for DEFENCE only
    write(stdout,'(6x,11(a,2x),2x,a)') 'A','B','C','D','E','F','G','H','I','J',GC//GC,'N'

  CASE ('o')
    write(stdout,'(12x,a)') 'OFFENCE'
    write(stdout,'(6x,11(a,1x))') 'A','B','C','D','E','F','G','H','I','J',GC

  CASE ('c')
    write(stdout,'(15x,a)') 'COMP'
    write(stdout,'(6x,11(a,2x))') 'A','B','C','D','E','F','G','H','I','J',GC

END SELECT

DO j=1,10
  write(stdout,'(3x,i2,1x)', ADVANCE='no') j
  DO i=1,10
    write(stdout,'(a,1x)', ADVANCE='no') a1(i,j)
  END DO

  ! 04-DEC-2017
  ! used to be case('d') and case('o':'c')
  ! Warning: Range specification at (1) can never be matched [-Wsurprising]
  ! switched to case('d') and case('c':'o')
  ! Error: CASE label at (1) overlaps with CASE label at (2)
  ! conclusion: don't use SELECT CASE with CHARACTER ranges, since they are technically numerical because ASCII
  IF( a == 'd' )                write(stdout,'(a)',ADVANCE='no') GSV//GSV ! defence
  IF( a == 'o' .OR. a == 'c' )  write(stdout,'(a)',ADVANCE='no') GSV ! offence OR comp

  IF(key) THEN

    SELECT CASE (j)

      CASE (1)
        write(stdout,'(4(1x,a))', ADVANCE='no') '<','W','E','>'

      CASE (2)
        write(stdout,'(4x,a)', ADVANCE='no') 'S'

      CASE (3)
        write(stdout,'(4x,a)', ADVANCE='no') 'v'
        key = .FALSE.

    END SELECT

  END IF

  write(stdout,*)

END DO

IF( a == 'd' ) THEN
  write(stdout,'(3x,a,1x)',ADVANCE='no') GC//GC
    DO i=1,10
      write(stdout,'(a,1x)',ADVANCE='no') GSH//GSH
    END DO
    write(stdout,'(a)',ADVANCE='no') GC//GC

ELSE IF( a == 'o' .OR. a == 'c' ) THEN
  write(stdout,'(3x,a,1x)',ADVANCE='no') GC//GC
    DO i=1,10
      write(stdout,'(a,1x)',ADVANCE='no') GSH
    END DO
    write(stdout,'(a)',ADVANCE='no') GC

END IF

write(stdout,*)

END SUBROUTINE grid_1

SUBROUTINE setship( boat, array )

INTEGER :: i
CHARACTER (LEN=*), DIMENSION(:,:), INTENT(INOUT) :: array
TYPE(ship), INTENT(IN) :: boat

array( boat%x, boat%y ) = boat%nu // boat%s ! // concatenates (joins) two stings together

SELECT CASE (boat%z)

  CASE ('N')
    DO i=1,boat%l-1
      array(boat%x,boat%y+i) = boat%nl // boat%s
    END DO

  CASE ('E')
    DO i=1,boat%l-1
      array(boat%x-i,boat%y) = boat%nl // boat%s
    END DO

  CASE ('S')
    DO i=1,boat%l-1
      array(boat%x,boat%y-i) = boat%nl // boat%s
    END DO

  CASE ('W')
    DO i=1,boat%l-1
      array(boat%x+i,boat%y) = boat%nl // boat%s
    END DO

END SELECT

END SUBROUTINE setship

SUBROUTINE clearship(boat,array)

INTEGER :: i
CHARACTER (LEN=*), DIMENSION(:,:), INTENT(INOUT) :: array
TYPE(ship), INTENT(IN) :: boat

IF(boat%z /= CN) THEN

  array(boat%x,boat%y) = CB//CB

  SELECT CASE (boat%z)

    CASE ('N')
      DO i=1,boat%l-1
        array(boat%x,boat%y+i) = CB//CB
      END DO

    CASE ('E')
      DO i=1,boat%l-1
        array(boat%x-i,boat%y) = CB//CB
      END DO

    CASE ('S')
      DO i=1,boat%l-1
        array(boat%x,boat%y-i) = CB//CB
      END DO

    CASE ('W')
      DO i=1,boat%l-1
        array(boat%x+i,boat%y) = CB//CB
      END DO

  END SELECT

END IF

END SUBROUTINE clearship

SUBROUTINE shipdata( boat, array, error_grid, error_collision )

TYPE(ship), INTENT(INOUT) :: boat
CHARACTER (LEN=*), DIMENSION(:,:), INTENT(IN) :: array
INTEGER, INTENT(OUT) :: error_grid, error_collision
CHARACTER (LEN=3) :: ans
INTEGER :: ax, ay1, ay2, c, i

error_grid = 0; error_collision = 0

DO

  DO
    write(stdout,'(a,1x)', ADVANCE='no') "<I> Enter x- and y- coordinates of ship's bow, from A1 to J10 [A-J 1-10]:"
    read(stdin,'(a3)', iostat=i) ans ! e.g. ans = A1 to A10, B1 to B10 etc.
    if(i == iostat_end) stop
    if(i /= 0) cycle
    c = 0; ax = IACHAR(ans(1:1)); ay1 = IACHAR(ans(2:2)); ay2 = IACHAR(ans(3:3))
    IF(ax >= 65 .AND. ax <= 74 .OR. ax >= 97 .AND. ax <= 106) c = c + 1 ! 65-74 = 'A'-'J', 97-106 = 'a'-'j'
    IF(ay1 >= 49 .AND. ay1 <= 57) c = c + 1 ! 49 = '1', 57 = '9'
    IF(ay2 == 32 .OR. (ay2 == 48 .AND. ay1 == 49)) c = c + 1 ! 32 = ' ' blank, 48 = '0'
    IF(c == 3) EXIT
    !print '(a)', '<ERROR> Try again.'
  END DO

  CALL convert_char2num(ans(1:1),boat%x)
  CALL convert_charnum2num(ans(2:3),boat%y)

  DO
    write(stdout,'(a,1x)', ADVANCE='no') "<I> Enter direction of ship's bow [N,E,S,W]:"
    read(stdin,'(a1)',iostat=i) boat%z
    if(i == iostat_end) stop
    if(i /= 0) cycle
    select case (boat%z)
    case('N','E','S','W', 'n','e','s','w')
      EXIT
    end select
  END DO

  CALL toUpper( boat%z )
  CALL checkship_grid (boat, error_grid )
  CALL checkship_collision( boat,array, error_collision )

  IF(error_grid == 0 .AND. error_collision == 0) EXIT

  IF(error_grid > 0) THEN
    print '(a,i2)', '<ERROR> Ship is off the grid.'
  END IF
  IF(error_collision > 0) THEN
    print '(a,i2)', '<ERROR> Ship collides with existing ships.'
  END IF

  DO
    write(stdout,'(a,1x)', ADVANCE='no') 'Try again? [Y/N]:'
    read(stdin,'(a1)', iostat=i) ans
    if(i == iostat_end) stop
    if(i /= 0) cycle
    IF(ans == 'n'.OR. ans == 'N'.or.ans == 'y'.OR. ans == 'Y') EXIT
  END DO

  IF(ans == 'n'.OR. ans == 'N') EXIT

END DO

END SUBROUTINE shipdata

SUBROUTINE checkship_grid( boat, c )

TYPE(ship), INTENT(IN) :: boat
INTEGER, INTENT(OUT) :: c
INTEGER :: i

c = 0

SELECT CASE (boat%z)

  CASE ('N')
    DO i = 1, boat%l - 1
      IF(boat%y + i > 10) THEN
        c = c + 1
        EXIT
      END IF
    END DO

  CASE ('E')
    DO i = 1, boat%l - 1
      IF(boat%x - i < 1) THEN
        c = c + 1
        EXIT
      END IF
    END DO

  CASE ('S')
    DO i = 1, boat%l - 1
      IF( boat%y - i < 1 ) THEN
        c = c + 1
        EXIT
      END IF
    END DO

  CASE ('W')
    DO i = 1, boat%l - 1
      IF( boat%x + i > 10 ) THEN
        c = c + 1
        EXIT
      END IF
    END DO

  CASE DEFAULT
    error STOP 'checkship_grid'

END SELECT

END SUBROUTINE checkship_grid

SUBROUTINE checkship_collision( boat, array, c )

TYPE(ship), INTENT(IN) :: boat
CHARACTER (LEN=*), DIMENSION(:,:), INTENT(IN) :: array
INTEGER :: i
INTEGER, INTENT(OUT) :: c

c = 0

! LLT(string_a,string_b) returns the value TRUE if string_b follows string_a
!   in the ASCII collating sequence, and false otherwise.

! CB is expected to be a sensible character such as '-' or ' ', which appears before the letter a-z and A-Z in the ASCII sequence

DO i = 0, boat%l - 1

  SELECT CASE ( boat%z )

    CASE ('N')
      IF( LLT( CB, array( boat%x, boat%y + i )(1:1) ) ) c = c + 1

    CASE ('E')
      IF( LLT( CB, array( boat%x - i, boat%y )(1:1) ) ) c = c + 1

    CASE ('S')
      IF( LLT( CB, array( boat%x, boat%y - i )(1:1) ) ) c = c + 1

    CASE ('W')
      IF( LLT( CB, array( boat%x + i, boat%y )(1:1) ) ) c = c + 1

  END SELECT

END DO

END SUBROUTINE checkship_collision

SUBROUTINE ranship(boats,array)
! set ship data (x,y,z) such that all ships are on the grid and no ships overlap
! begin with longest ship, and fit others around existing ships
INTEGER :: i, c1, c2
REAL, DIMENSION(3) :: ran
CHARACTER (LEN=*), DIMENSION(:,:), INTENT(INOUT) :: array
TYPE(ship), DIMENSION(:), INTENT(INOUT) :: boats
INTEGER, DIMENSION(SIZE(boats)) :: lengths

  ! lengths(1:5) = (/ l1 /)
  !                (/ l2 /)
  !                (/ l3 /)
  !                (/ l4 /)
  !                (/ l5 /)

DO i=1,SIZE(boats)
  lengths(i) = boats(i)%l
END DO

!DO i=1,5
!  write(stdout,'(1x,i2)', ADVANCE='no') lengths(i)
!END DO
!write(stdout,*)

CALL sort_1d(lengths,'d')
!STOP

! allocate ships in order of descending length

DO i=1,SIZE(lengths) ! variable ship loop
  DO ! error checking loop

    CALL RANDOM_NUMBER(ran)
    ran = ran*10.0 + 1.0
    DO ! direction can only be [1,2,3,4] = [n,e,s,w]
      IF(INT(ran(3)) >= 1 .AND. INT(ran(3)) <= 8) EXIT
      CALL RANDOM_NUMBER(ran(3))
      ran(3) = ran(3)*10.0 + 1.0
    END DO

    ! allocate random (x,y,z) to the boat with the largest length first
    ! checking for grid errors and collision errors in the process
    boats(i)%x = INT(ran(1)); boats(i)%y = INT(ran(2))
    SELECT CASE (INT(ran(3)))
      CASE (1:2)
        boats(i)%z = 'N'
      CASE (3:4)
        boats(i)%z = 'E'
      CASE (5:6)
        boats(i)%z = 'S'
      CASE (7:8)
        boats(i)%z = 'W'
    END SELECT
    CALL checkship_grid(boats(i),c1)
    CALL checkship_collision(boats(i),array,c2)
    IF(c1 == 0 .AND. c2 == 0) THEN
      CALL setship(boats(i),array)
    END IF

    IF(c1 == 0 .AND. c2 == 0) EXIT

  END DO
END DO

END SUBROUTINE ranship

SUBROUTINE identify(t,boats,i,s)

INTEGER, INTENT(OUT) :: i
LOGICAL, INTENT(OUT) :: s
TYPE(target), INTENT(INOUT) :: t
TYPE(ship), DIMENSION(:), INTENT(INOUT) :: boats

! Identify which ship was hit and determine if it has been sunk.

! t%s(1:1) identifies ship by initial; need to convert to upper
CALL toUpper(t%s(1:1))

SELECT CASE (t%s(1:1))
  CASE ('D')
    i = 1
  CASE ('C')
    i = 2
  CASE ('S')
    i = 3
  CASE ('B')
    i = 4
  CASE ('A')
    i = 5
END SELECT

boats(i)%h = boats(i)%h + 1 ! ship has been hit
IF(boats(i)%h == boats(i)%l) THEN ! is ship sunk?
  s = .TRUE.
ELSE
  s = .FALSE.
END IF

END SUBROUTINE identify

SUBROUTINE display_target(i1,i2)

INTEGER, INTENT(IN) :: i1,i2
CHARACTER(LEN=2) :: c1*1, c2

CALL convert_num2char(i1,c1)
WRITE(c2,'(i2)') i2
write(stdout,'(3a)') 'Target: ',c1,ADJUSTL(c2)

END SUBROUTINE display_target

SUBROUTINE ai(defence,enemy,enemysunk,hits,hits_enemy,delay,ai_data,health,offence) ! artificial intelligence = player 0

INTEGER :: hit_ship = 0, i, c
INTEGER, INTENT(IN) :: hits_enemy ! identifies which hits scoreboard entity to use
REAL, DIMENSION(1:2) :: random
CHARACTER (LEN=2), DIMENSION(1:10,1:10), INTENT(INOUT) :: defence
CHARACTER (LEN=1), DIMENSION(1:10,1:10), INTENT(INOUT), OPTIONAL :: offence
LOGICAL :: sunk, debug = .FALSE.
LOGICAL, INTENT(IN) :: delay
LOGICAL, DIMENSION(1:5), INTENT(INOUT) :: enemysunk
TYPE(target) :: p0t
TYPE(ship), DIMENSION(1:5), INTENT(INOUT) :: enemy
TYPE(scoreboard_entity), DIMENSION(1:5), INTENT(INOUT), OPTIONAL :: health
TYPE(scoreboard_entity), DIMENSION(1:2), INTENT(INOUT) :: hits
TYPE(ai_saved_data), INTENT(INOUT) :: ai_data

IF(ai_data%first_time) THEN
  ai_data%first_time = .FALSE.
  ai_data%stage = 1
END IF

! ### AI START ###

IF(debug) write(stdout,'(a,i1)') 'stage = ',ai_data%stage

SELECT CASE(ai_data%stage)

  CASE (1) ! New Target

    DO
      CALL RANDOM_NUMBER( random ); random = random*10.0 + 1.0
      p0t%x = INT(random(1)); p0t%y = INT(random(2))
      p0t%s = defence( p0t%x, p0t%y ) ! query array at point for complete state
      IF( p0t%s(2:2) == CB ) EXIT ! not already fired at this position
    END DO

    CALL display_target( p0t%x, p0t%y )
    IF(delay) CALL sleep(500)

    IF( p0t%s(1:1) == CB ) THEN

      print '(a)', 'Miss!'
      defence(p0t%x,p0t%y)(2:2) = CM
      IF(PRESENT(offence)) THEN
      offence(p0t%x,p0t%y) = CM
      END IF

    ELSE

      print '(a)', 'Hit!'
      defence(p0t%x,p0t%y)(2:2) = CH
      IF( PRESENT( offence ) ) offence(p0t%x,p0t%y) = CH
      ! Identify which ship was hit and determine if it has been sunk.
      CALL identify( p0t, enemy, hit_ship, sunk )
      enemysunk(hit_ship) = sunk
      IF( PRESENT( health ) ) health(hit_ship)%s = health(hit_ship)%s - 1

      IF(sunk) THEN ! If sunk then goto stage 1

        IF(delay) CALL sleep(500)
        write(stdout,'(3a)') "I've sunk your ",TRIM(enemy( hit_ship )%n),"!"
        hits( hits_enemy )%s = hits( hits_enemy )%s + 1
        ai_data%stage = 1

      ELSE ! If not sunk then record position of target

        ai_data%centre(1) = p0t%x; ai_data%centre(2) = p0t%y
        ai_data%stage = 2; ai_data%scan_stage = 1 ! on next turn, enter CASE (2)

      END IF

    END IF

  CASE (2) ! stage
    ! A ship has been hit, begin firing at adjacent spaces until another hit is made, or all are miss/already fired at.
    ! start with a random valid adjacent position, then rotate clockwise/anticlockwise.
    ! give up when all possible directions have been tested.

    IF(debug) write(stdout,'(a,i1)') 'scan_stage = ', ai_data%scan_stage

    SELECT CASE( ai_data%scan_stage )

      CASE (1)
        ! array 'adjacent' holds the coords for positions adjacent to a given centre.        a(1)
        ! array analysed for positions off the grid, these will be set to -1.            a(4) C  a(2)
        ! array analysed for positions already fired at, these will be set to -2.            a(3)
        ! going clockwise = increasing index, anti-clockwise = decreasing index.

        ai_data%adjacent(1,1) = ai_data%centre(1)     ; ai_data%adjacent(1,2) = ai_data%centre(2) - 1 ! north
        ai_data%adjacent(2,1) = ai_data%centre(1) + 1 ; ai_data%adjacent(2,2) = ai_data%centre(2)     ! east
        ai_data%adjacent(3,1) = ai_data%centre(1)     ; ai_data%adjacent(3,2) = ai_data%centre(2) + 1 ! south
        ai_data%adjacent(4,1) = ai_data%centre(1) - 1 ; ai_data%adjacent(4,2) = ai_data%centre(2)     ! west

        DO i=1,4

          IF(ai_data%adjacent(i,1) >= 1 .AND. ai_data%adjacent(i,1) <= 10 .AND. &
            &  ai_data%adjacent(i,2) >= 1 .AND. ai_data%adjacent(i,2) <= 10) CYCLE

          ai_data%adjacent(i,1:2) = -1 ! points off the grid will be ignored

        END DO

        DO i=1,4

          IF( ai_data%adjacent(i,1) /= -1 ) THEN ! only points on the grid are considered

            p0t%x = ai_data%adjacent(i,1); p0t%y = ai_data%adjacent(i,2)
            p0t%s = defence( p0t%x, p0t%y )
            IF( p0t%s(2:2) /= CB ) ai_data%adjacent(i,1:2) = -2 ! points already fired at are ignored

          END IF

        END DO

        IF(debug) THEN
          print '(a)', 'adjacent ='

          DO i=1,4
            print '(2(1x,i2))', ai_data%adjacent(i,1:2)
          END DO

        END IF

        ! check if all of adjacent is negative; if so, skip firing and return to stage 1
        c = 0 ! c = number of valid positions [1,2,3,4]

        DO i=1,4
          IF( ai_data%adjacent(i,1) > 0 ) c = c + 1
        END DO

        IF(debug) print '(a,i2)', 'c = ',c

        IF(c /= 0) THEN ! there is at least one valid point in the array

          ! select start position from valid positions in array
          DO
            DO ! start can only be [1,2,3,4] = [n,e,s,w]
              CALL RANDOM_NUMBER( random(1) ); random(1) = random(1)*10.0 + 1.0
              IF( INT(random(1)) >= 1 .AND. INT(random(1)) <= 4 ) EXIT
            END DO
            IF( ai_data%adjacent( INT(random(1)), 1 ) >= 1 ) EXIT ! only valid points are selected
          END DO

          ai_data%start = INT(random(1))

          ! select a random direction to rotate for subsequent firing around centre point
          DO ! clock can only be [1,2=-1] = [c,a]
            CALL RANDOM_NUMBER( random(2) ); random(2) = random(2)*10.0 + 1.0
            IF( INT(random(2)) >= 1 .AND. INT(random(2)) <= 2 ) EXIT
          END DO

          ai_data%clock = INT(random(2))
          IF(ai_data%clock == 2) ai_data%clock = -1 ! for anticlockwise rotation

          IF(debug) print '(a,i2)', 'start = ',ai_data%start
          IF(debug) print '(a,i2)', 'clock = ',ai_data%clock

          p0t%x = ai_data%adjacent( ai_data%start, 1 ); p0t%y = ai_data%adjacent( ai_data%start, 2 ) ! fire at start position
          p0t%s = defence( p0t%x, p0t%y )

          CALL display_target( p0t%x, p0t%y )
          IF(delay) CALL sleep(500)

          IF(p0t%s(1:1) == CB) THEN

            print '(a)', 'Miss!'
            defence( p0t%x, p0t%y )(2:2) = CM
            IF( PRESENT(offence) ) offence(p0t%x,p0t%y) = CM
            IF(c > 1) THEN ! check for c > 1 because start has taken first point
              ai_data%scan_stage = 2; i = 1 ! continue firing around centre
            ELSE ! c = 1, there was only one point to test, which has already been tested by start.
              ai_data%stage = 1 ! return to random firing
            END IF

          ELSE

            print '(a)', 'Hit!'
            defence( p0t%x, p0t%y )(2:2) = CH
            IF( PRESENT(offence) ) offence(p0t%x,p0t%y) = CH
            ! Identify which ship was hit and determine if it has been sunk.
            CALL identify( p0t, enemy, hit_ship, sunk )
            enemysunk( hit_ship ) = sunk
            IF( PRESENT(health) ) health( hit_ship )%s = health( hit_ship )%s - 1

            IF(sunk) THEN
              IF(delay) CALL sleep(500)
              print '(3a)', "I've sunk your ",TRIM(enemy( hit_ship )%n),"!"
              hits( hits_enemy )%s = hits (hits_enemy )%s + 1
              ai_data%stage = 1
            ELSE
              ai_data%stage = 3; ai_data%scan_stage = 1; ai_data%pos = ai_data%start ! must define pos
            END IF

          END IF

        ELSE
          ai_data%stage = 1
        END IF

      CASE (2) ! scan_stage : start has missed, rotate around centre, knowing there is at least one valid point

        ! update number of valid positions
        DO i=1,4

          IF( ai_data%adjacent(i,1) >= 1 .AND. ai_data%adjacent(i,1) <= 10 .AND. &
            &  ai_data%adjacent(i,2) >= 1 .AND. ai_data%adjacent(i,2) <= 10 ) CYCLE

          ai_data%adjacent(i,1:2) = -1 ! points off the grid will be ignored

        END DO

        DO i=1,4

          IF( ai_data%adjacent(i,1) /= -1 ) THEN ! only points on the grid are considered

            p0t%x = ai_data%adjacent(i,1); p0t%y = ai_data%adjacent(i,2)
            p0t%s = defence( p0t%x, p0t%y )
            IF( p0t%s(2:2) /= CB) ai_data%adjacent(i,1:2) = -2 ! points already fired at are ignored

          END IF

        END DO

        c = 0 ! c = number of valid positions [1,2,3,4]

        DO i=1,4
          IF(ai_data%adjacent(i,1) > 0) c = c + 1
        END DO

        IF(debug) print '(a,i2)', 'c = ',c ! c should be at least 1 here

        i = 1
        DO
          ai_data%pos = ai_data%start + ai_data%clock*i
          IF( ai_data%pos > 4 ) ai_data%pos = ai_data%pos - 4 ! wrap around
          IF( ai_data%pos < 1 ) ai_data%pos = ai_data%pos + 4
          IF(debug) print '(a,i2)', 'pos = ',ai_data%pos
          IF( ai_data%adjacent( ai_data%pos, 1 ) >= 1 .OR. i == 4 ) EXIT
          i = i + 1
        END DO

        IF(debug) print '(a,i2)', 'i = ',i ! if i=4 then an error has occurred

        IF( ai_data%adjacent( ai_data%pos , 1 ) >= 1 ) THEN ! the next valid point has been found

          p0t%x = ai_data%adjacent( ai_data%pos, 1 ); p0t%y = ai_data%adjacent( ai_data%pos , 2 ) ! fire at current position
          p0t%s = defence( p0t%x, p0t%y )
          CALL display_target( p0t%x, p0t%y )
          IF(delay) CALL sleep(500)

          IF(p0t%s(1:1) == CB) THEN

            print '(a)', 'Miss!'
            defence( p0t%x, p0t%y )(2:2) = CM
            IF(PRESENT(offence)) offence(p0t%x,p0t%y) = CM
            IF(c == 1) ai_data%stage = 1 ! found the only valid point
            ! ELSE continue firing around centre

          ELSE

            print '(a)', 'Hit!'
            defence( p0t%x, p0t%y )(2:2) = CH
            IF( PRESENT(offence) ) offence( p0t%x, p0t%y ) = CH
            ! Identify which ship was hit and determine if it has been sunk.
            CALL identify( p0t, enemy, hit_ship, sunk )
            enemysunk( hit_ship ) = sunk
            IF( PRESENT(health) ) health( hit_ship )%s = health( hit_ship )%s - 1

            IF(sunk) THEN

              IF(delay) CALL sleep(500)
              print '(3a)', "I've sunk your ",TRIM( enemy( hit_ship )%n ),"!"
              hits(hits_enemy)%s = hits(hits_enemy)%s + 1
              ai_data%stage = 1

            ELSE

              ai_data%stage = 3; ai_data%scan_stage = 1!; i = 2; pass = 1

            END IF

          END IF

        END IF

    END SELECT

  CASE (3) ! stage : Iterations of firing in one direction, then the other.

    ! Two hits in a row, the ship has not been sunk. Construct an array similar to adjacent that contains available points
    ! which is then whittled down to only valid points by disregarding points that are off the grid or already fired at.
    IF(debug) print '(a,i1)', 'scan_stage = ',ai_data%scan_stage

    SELECT CASE(ai_data%scan_stage)

      CASE (1) ! set up line array and fire at first available point

        ai_data%line(0,1:2) = ai_data%centre
        IF(debug) print '(a,i2)', 'pos = ',ai_data%pos

        SELECT CASE(ai_data%pos)

          CASE (0)
            error STOP 'pos'

          CASE (1) ! north
            ai_data%line(-4:4,1) = ai_data%centre(1) ! same x value for all points on vertical line

            DO i=-4,4
              ai_data%line(i,2) = ai_data%centre(2) - i ! different y values
            END DO

          CASE (2) ! east
            ai_data%line(-4:4,2) = ai_data%centre(2) ! same y value for all points on horizontal line

            DO i=-4,4
              ai_data%line(i,1) = ai_data%centre(1) + i ! different x values
            END DO

          CASE (3) ! south
            ai_data%line(-4:4,1) = ai_data%centre(1) ! same x value for all points on vertical line

            DO i=-4,4
              ai_data%line(i,2) = ai_data%centre(2) + i ! different y values
            END DO

          CASE (4) ! west
            ai_data%line(-4:4,2) = ai_data%centre(2) ! same y value for all points on horizontal line

            DO i=-4,4
              ai_data%line(i,1) = ai_data%centre(1) - i ! different x values
            END DO

        END SELECT

        IF(debug) THEN
          print '(a)', 'line ='
          DO i=-4,4
            print '(3(1x,i2))',  i, ai_data%line(i,1:2)
          END DO
        END IF

        ! find number of valid positions
        DO i=-4,4

          IF(ai_data%line(i,1) >= 1 .AND. ai_data%line(i,1) <= 10 .AND. &
            &  ai_data%line(i,2) >= 1 .AND. ai_data%line(i,2) <= 10) CYCLE

          ai_data%line(i,1:2) = -1 ! points off the grid will be ignored

        END DO

        DO i=-4,4

          IF(ai_data%line(i,1) /= -1) THEN ! only points on the grid are considered

            p0t%x = ai_data%line(i,1); p0t%y = ai_data%line(i,2)
            p0t%s = defence(p0t%x,p0t%y)

            IF( p0t%s(2:2) /= CB ) THEN
              ai_data%line(i,1:2) = -2 ! points already fired at are ignored
              ! all points beyond here are also discarded
              !IF(i < 0) ai_data%line(-4:i,1:2) = -2
              !IF(i > 1) ai_data%line(i:4,1:2) = -2
            END IF

          END IF

        END DO

        IF(debug) THEN
          print '(a)', 'line ='
          DO i=-4,4
            print '(3(1x,i2))',  i, ai_data%line(i,1:2)
          END DO
        END IF

        c = 0 ! c = number of valid positions [1,2,3,4]

        DO i=-4,4
          IF(ai_data%line(i,1) > 0) c = c + 1
        END DO

        IF(debug) print '(a,i2)', 'c = ',c ! c=1:7

        IF(c == 0) error STOP 'c'

        ai_data%clock = 1; i = 2 ! i = 0 and 1 already done

        DO
          ai_data%line_pos = ai_data%clock*i

          IF(ai_data%line_pos > 4 .OR. ai_data%line_pos < -4) THEN ! about-face at end of range
            ai_data%clock = -1*ai_data%clock; i = 1; ai_data%line_pos = ai_data%clock*i
          END IF

          IF(debug) print '(a,i2)', 'line_pos1 = ',ai_data%line_pos

          IF(ai_data%line(ai_data%line_pos,1) >= 1 .OR. i == 5) EXIT
          i = i + 1
        END DO

        IF(debug) print '(a,i2)', 'i = ',i ! if i=5 then an error has occurred

        IF( ai_data%line( ai_data%line_pos , 1 ) >= 1 ) THEN ! the next valid point has been found

          p0t%x = ai_data%line( ai_data%line_pos , 1 ); p0t%y = ai_data%line( ai_data%line_pos , 2 ) ! fire at current position
          p0t%s = defence( p0t%x, p0t%y )

          CALL display_target( p0t%x, p0t%y )
          IF(delay) CALL sleep(500)

          IF( p0t%s(1:1) == CB ) THEN

            print '(a)', 'Miss!'
            defence( p0t%x, p0t%y )(2:2) = CM
            IF( PRESENT(offence) ) offence( p0t%x, p0t%y ) = CM

            IF(c == 1) THEN ! found the only valid point
              ai_data%stage = 1
            ELSE
              ai_data%scan_stage = 2
            END IF

          ELSE

            print '(a)', 'Hit!'
            defence( p0t%x, p0t%y )(2:2) = CH
            IF( PRESENT(offence) ) offence( p0t%x, p0t%y ) = CH
            ! Identify which ship was hit and determine if it has been sunk.
            CALL identify( p0t, enemy, hit_ship, sunk )
            enemysunk( hit_ship ) = sunk
            IF( PRESENT(health) ) health(hit_ship)%s = health(hit_ship)%s - 1

            IF(sunk) THEN

              IF(delay) CALL sleep(500)
              print '(3a)', "I've sunk your ",TRIM( enemy( hit_ship )%n ),"!"
              hits( hits_enemy )%s = hits( hits_enemy )%s + 1
              ai_data%stage = 1

            ELSE
              ai_data%stage = 3; ai_data%scan_stage = 1

            END IF

          END IF

        END IF
        !STOP

      CASE (2) ! scan_stage : continue firing along line array until all valid points have been exhausted, after aboutface

        ! update  line array
        ai_data%line(0,1:2) = ai_data%centre

        IF(debug) print '(a,i2)', 'pos = ',ai_data%pos

        SELECT CASE( ai_data%pos )

          CASE (0)
            error STOP 'pos'

          CASE (1) ! north
            ai_data%line(-4:4,1) = ai_data%centre(1) ! same x value for all points on vertical line

            DO i=-4,4
              ai_data%line(i,2) = ai_data%centre(2) - i ! different y values
            END DO

          CASE (2) ! east
            ai_data%line(-4:4,2) = ai_data%centre(2) ! same y value for all points on horizontal line

            DO i=-4,4
              ai_data%line(i,1) = ai_data%centre(1) + i ! different x values
            END DO

          CASE (3) ! south
            ai_data%line(-4:4,1) = ai_data%centre(1) ! same x value for all points on vertical line

            DO i=-4,4
              ai_data%line(i,2) = ai_data%centre(2) + i ! different y values
            END DO

          CASE (4) ! west
            ai_data%line(-4:4,2) = ai_data%centre(2) ! same y value for all points on horizontal line
            DO i=-4,4

              ai_data%line(i,1) = ai_data%centre(1) - i ! different x values
            END DO

        END SELECT

        IF(debug) THEN
          print '(a)', 'line ='
          DO i=-4,4
            print '(3(1x,i2))',  i, ai_data%line(i,1:2)
          END DO
        END IF

        ! find number of valid positions
        DO i=-4,4

          IF( ai_data%line(i,1) >= 1 .AND. ai_data%line(i,1) <= 10 .AND. &
            &  ai_data%line(i,2) >= 1 .AND. ai_data%line(i,2) <= 10 .OR. i > 0 ) CYCLE ! i > 0 because already aboutface

          ai_data%line(i,1:2) = -1 ! points off the grid will be ignored

        END DO

        DO i=-4,4

          IF( ai_data%line(i,1) /= -1 ) THEN ! only points on the grid are considered

            p0t%x = ai_data%line(i,1); p0t%y = ai_data%line(i,2)
            p0t%s = defence(p0t%x,p0t%y)

            IF( p0t%s(2:2) /= CB ) ai_data%line(i,1:2) = -2 ! points already fired at are ignored

          END IF

        END DO

        IF(debug) THEN
          print '(a)', 'line ='
          DO i=-4,4
            print '(3(1x,i2))',  i, ai_data%line(i,1:2)
          END DO
        END IF

        c = 0 ! c = number of valid positions [1,2,3,4]
        DO i=-4,4
          IF( ai_data%line(i,1) > 0 .AND. i < 0 ) c = c + 1
        END DO
        IF(debug) print '(a,i2)', 'c = ',c ! c=1:7

        ai_data%clock = -1; i = 1
        DO
          ai_data%line_pos = ai_data%clock*i

          IF( ai_data%line_pos > 4 .OR. ai_data%line_pos < -4 ) THEN ! about-face at end of range
            ai_data%clock = -1*ai_data%clock; i = 1; ai_data%line_pos = ai_data%clock*i
          END IF

          IF(debug) print '(a,i2)', 'line_pos2 = ',ai_data%line_pos

          IF( ai_data%line(ai_data%line_pos,1) >= 1 .OR. i == 5 ) EXIT
          i = i + 1
        END DO
        IF(debug) print '(a,i2)', 'i = ',i ! if i=5 then an error has occurred

        IF( ai_data%line( ai_data%line_pos, 1 ) >= 1 ) THEN ! the next valid point has been found

          p0t%x = ai_data%line( ai_data%line_pos, 1 ); p0t%y = ai_data%line( ai_data%line_pos, 2 ) ! fire at current position
          p0t%s = defence(p0t%x,p0t%y)

          CALL display_target( p0t%x,p0t%y )
          IF(delay) CALL sleep(500)

          IF( p0t%s(1:1) == CB ) THEN

            print '(a)', 'Miss!'
            defence(p0t%x,p0t%y)(2:2) = CM
            IF(PRESENT(offence)) THEN
              offence(p0t%x,p0t%y) = CM
            END IF

            ai_data%stage = 1
            !STOP 'miss'

          ELSE

            print '(a)', 'Hit!'
            defence( p0t%x, p0t%y )(2:2) = CH
            IF( PRESENT(offence) ) offence(p0t%x,p0t%y) = CH
            ! Identify which ship was hit and determine if it has been sunk.
            CALL identify( p0t, enemy, hit_ship, sunk )
            enemysunk( hit_ship) = sunk
            IF( PRESENT(health) ) health( hit_ship )%s = health( hit_ship )%s - 1

            IF(sunk) THEN
              IF(delay) CALL sleep(500)
              print '(3a)', "I've sunk your ",TRIM( enemy( hit_ship )%n ),"!"
              hits( hits_enemy )%s = hits( hits_enemy )%s + 1
              ai_data%stage = 1
            END IF

          END IF

        END IF

        !STOP 'scan_stage'
    END SELECT
END SELECT

! ### AI END ###

END SUBROUTINE ai

END MODULE battleships_mod
