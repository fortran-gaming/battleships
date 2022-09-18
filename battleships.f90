!=======================================================================
! psq95, Univeristy of Surrey, 2013
! Latest build: Dec 2017
!
! References
! 1. ASCII font 'cybermedium' http://www.topster.de/text-to-ascii/cybermedium.html
!========================================================================

MODULE misc_mod

use iso_fortran_env, only : stdin => input_unit

implicit none

CONTAINS

SUBROUTINE ranseed(operation,filename) ! add output to record a_seed in seedbank.dat

INTEGER :: i_seed, i, ios,u
INTEGER, DIMENSION(:), ALLOCATABLE :: a_seed
INTEGER, DIMENSION(1:8) :: dt_seed
CHARACTER(LEN=*), INTENT(IN) :: operation, filename

SELECT CASE(operation)
  CASE ('save')
    CALL RANDOM_SEED(size=i_seed) ! get required size of array
    ALLOCATE(a_seed(1:i_seed))
    CALL RANDOM_SEED(get=a_seed) ! get old seed
    CALL DATE_AND_TIME(values=dt_seed) ! get some random numbers
    a_seed(i_seed)=dt_seed(8); a_seed(1)=dt_seed(8)*dt_seed(7)*dt_seed(6) ! modify the old seed
    CALL RANDOM_SEED(put=a_seed) ! replace seed

    OPEN(newunit=u,FILE=filename,STATUS='replace')
    WRITE(u,'(i0)') i_seed
    WRITE(u,*)
    WRITE(u,'(i0)') a_seed
    CLOSE(u)

    print '(a,i0,a)', 'Saved seed ',a_seed(1),' to "'//filename//'"'

    DEALLOCATE(a_seed)

  CASE ('load')
    OPEN(newunit=u,FILE=filename,STATUS='old',IOSTAT=ios)
    IF( ios /= 0 ) error STOP 'error: ranseed: could not open "'//filename//'"'
    READ(u,*) i_seed
    ALLOCATE(a_seed(1:i_seed))
    READ(u,*)
    DO i=1,i_seed
      READ(u,*) a_seed(i)
    END DO
    CLOSE(u)

    CALL RANDOM_SEED(put=a_seed)

    print '(a,i0,a)', 'Loaded seed ',a_seed(1),' from "'//filename//'"'

    DEALLOCATE(a_seed)

  CASE DEFAULT
    error STOP 'error: ranseed: invalid operation'

END SELECT

END SUBROUTINE ranseed

SUBROUTINE sort_1d(l,o)

INTEGER, DIMENSION(:), INTENT(INOUT) :: l
CHARACTER (LEN=1), INTENT(IN) :: o
INTEGER, DIMENSION(:), ALLOCATABLE :: p
INTEGER :: v = 0
INTEGER :: i, w, c

ALLOCATE(p(SIZE(l)))
p = l
! The Algorithm from James Measures (adapted)
DO i=SIZE(l),1,-1
  SELECT CASE (o)
    CASE ('a')
      v = MAXVAL(l(1:i))
      l(MAXLOC(l(1:i))) = l(i) ! swap max/min value with last value in i range
    CASE ('d')
      v = MINVAL(l(1:i))
      l(MINLOC(l(1:i))) = l(i) ! swap max/min value with last value in i range
  END SELECT
  l(i) = v
  c = 0
  DO w=1,SIZE(l)
    IF(l(w) == p(w)) THEN
      c = c + 1
    END IF
  END DO
  IF(c == SIZE(l)) EXIT ! no change from previous iteration
  p = l
END DO
END SUBROUTINE sort_1d

pure SUBROUTINE convert_char2num(c,i)

CHARACTER (LEN=1), INTENT(INOUT) :: c
INTEGER, INTENT(OUT) :: i

IF(IACHAR(c) >= 97 .AND. IACHAR(c) <= 106) THEN ! if c = lowercase a-j
  c = ACHAR(IACHAR(c) - 32) ! convert to uppercase
END IF

SELECT CASE(c)
  CASE ('A')
    i = 1
  CASE ('B')
    i = 2
  CASE ('C')
    i = 3
  CASE ('D')
    i = 4
  CASE ('E')
    i = 5
  CASE ('F')
    i = 6
  CASE ('G')
    i = 7
  CASE ('H')
    i = 8
  CASE ('I')
    i = 9
  CASE ('J')
    i = 10
END SELECT

END SUBROUTINE convert_char2num

pure SUBROUTINE convert_charnum2num(c,i)

CHARACTER (LEN=2), INTENT(IN) :: c
INTEGER,INTENT(OUT) :: i

SELECT CASE(c(1:1))
  CASE ('1')
    IF(c(2:2) == ' ') THEN
      i = 1
    ELSE
      i = 10
    END IF
  CASE ('2')
    i = 2
  CASE ('3')
    i = 3
  CASE ('4')
    i = 4
  CASE ('5')
    i = 5
  CASE ('6')
    i = 6
  CASE ('7')
    i = 7
  CASE ('8')
    i = 8
  CASE ('9')
    i = 9
END SELECT

END SUBROUTINE convert_charnum2num

pure SUBROUTINE convert_num2char(i,c)

CHARACTER (LEN=1), INTENT(OUT) :: c
INTEGER, INTENT(IN) :: i

SELECT CASE(i)
  CASE (1)
    c = 'A'
  CASE (2)
    c = 'B'
  CASE (3)
    c = 'C'
  CASE (4)
    c = 'D'
  CASE (5)
    c = 'E'
  CASE (6)
    c = 'F'
  CASE (7)
    c = 'G'
  CASE (8)
    c = 'H'
  CASE (9)
    c = 'I'
  CASE (10)
    c = 'J'
END SELECT

END SUBROUTINE convert_num2char

SUBROUTINE toUpper(a)

CHARACTER (LEN=1), INTENT(INOUT) :: a

IF(IACHAR(a) >= 97 .AND. IACHAR(a) <= 122) THEN ! 97 <= a...z lowercase <= 122
  a = ACHAR(IACHAR(a) - 32) ! 65 <= A...Z uppercase <= 90
END IF

END SUBROUTINE toUpper

SUBROUTINE char2int(a,i)

CHARACTER (LEN=*), INTENT(IN) :: a
INTEGER, INTENT(OUT) :: i
CHARACTER (LEN=32) :: fmt_char, len_char

IF(IACHAR(a) >= 48 .AND. IACHAR(a) <= 57) THEN ! 48 <= 0 ... 9 <= 57
  WRITE(len_char,'(i0)') LEN(a)
  fmt_char = "(i"//TRIM(ADJUSTL(len_char))//")"
  !WRITE(6,*) 'fmt = ', fmt_char
  READ(a,fmt_char) i
ELSE
  i = -1
END IF

END SUBROUTINE char2int

SUBROUTINE strip_spaces( string )

  CHARACTER(LEN=*), INTENT(INOUT) :: string
  INTEGER :: string_len, last, actual

  string_len = len(string); last = 1; actual = 1

  DO WHILE( actual < string_len )
      IF( string(last:last) == ' ' ) THEN ! swap next char with space
          actual = actual + 1
          string(last:last) = string(actual:actual)
          string(actual:actual) = ' '
      ELSE
          last = last + 1
          IF( actual < last ) actual = last
      END IF
  END DO

END SUBROUTINE strip_spaces

END MODULE misc_mod

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
    IF(delay) CALL SLEEP(1)

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

        IF(delay) CALL SLEEP(1)
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
          IF(delay) CALL SLEEP(1)

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
              IF(delay) CALL SLEEP(1)
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

        IF(debug) WRITE(6,'(a,i2)') 'c = ',c ! c should be at least 1 here

        i = 1
        DO
          ai_data%pos = ai_data%start + ai_data%clock*i
          IF( ai_data%pos > 4 ) ai_data%pos = ai_data%pos - 4 ! wrap around
          IF( ai_data%pos < 1 ) ai_data%pos = ai_data%pos + 4
          IF(debug) WRITE(6,'(a,i2)') 'pos = ',ai_data%pos
          IF( ai_data%adjacent( ai_data%pos, 1 ) >= 1 .OR. i == 4 ) EXIT
          i = i + 1
        END DO

        IF(debug) WRITE(6,'(a,i2)') 'i = ',i ! if i=4 then an error has occurred

        IF( ai_data%adjacent( ai_data%pos , 1 ) >= 1 ) THEN ! the next valid point has been found

          p0t%x = ai_data%adjacent( ai_data%pos, 1 ); p0t%y = ai_data%adjacent( ai_data%pos , 2 ) ! fire at current position
          p0t%s = defence( p0t%x, p0t%y )
          CALL display_target( p0t%x, p0t%y )
          IF(delay) CALL SLEEP(1)

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

              IF(delay) CALL SLEEP(1)
              WRITE(6,'(3a)') "I've sunk your ",TRIM( enemy( hit_ship )%n ),"!"
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
    IF(debug) WRITE(6,'(a,i1)') 'scan_stage = ',ai_data%scan_stage

    SELECT CASE(ai_data%scan_stage)

      CASE (1) ! set up line array and fire at first available point

        ai_data%line(0,1:2) = ai_data%centre
        IF(debug) WRITE(6,'(a,i2)') 'pos = ',ai_data%pos

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
            WRITE(6,'(3(1x,i2))')  i, ai_data%line(i,1:2)
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
            WRITE(6,'(3(1x,i2))')  i, ai_data%line(i,1:2)
          END DO
        END IF

        c = 0 ! c = number of valid positions [1,2,3,4]

        DO i=-4,4
          IF(ai_data%line(i,1) > 0) c = c + 1
        END DO

        IF(debug) WRITE(6,'(a,i2)') 'c = ',c ! c=1:7

        IF(c == 0) error STOP 'c'

        ai_data%clock = 1; i = 2 ! i = 0 and 1 already done

        DO
          ai_data%line_pos = ai_data%clock*i

          IF(ai_data%line_pos > 4 .OR. ai_data%line_pos < -4) THEN ! about-face at end of range
            ai_data%clock = -1*ai_data%clock; i = 1; ai_data%line_pos = ai_data%clock*i
          END IF

          IF(debug) WRITE(6,'(a,i2)') 'line_pos1 = ',ai_data%line_pos

          IF(ai_data%line(ai_data%line_pos,1) >= 1 .OR. i == 5) EXIT
          i = i + 1
        END DO

        IF(debug) WRITE(6,'(a,i2)') 'i = ',i ! if i=5 then an error has occurred

        IF( ai_data%line( ai_data%line_pos , 1 ) >= 1 ) THEN ! the next valid point has been found

          p0t%x = ai_data%line( ai_data%line_pos , 1 ); p0t%y = ai_data%line( ai_data%line_pos , 2 ) ! fire at current position
          p0t%s = defence( p0t%x, p0t%y )

          CALL display_target( p0t%x, p0t%y )
          IF(delay) CALL SLEEP(1)

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

              IF(delay) CALL SLEEP(1)
              WRITE(6,'(3a)') "I've sunk your ",TRIM( enemy( hit_ship )%n ),"!"
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

        IF(debug) WRITE(6,'(a,i2)') 'pos = ',ai_data%pos

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
            WRITE(6,'(3(1x,i2))')  i, ai_data%line(i,1:2)
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
            WRITE(6,'(3(1x,i2))')  i, ai_data%line(i,1:2)
          END DO
        END IF

        c = 0 ! c = number of valid positions [1,2,3,4]
        DO i=-4,4
          IF( ai_data%line(i,1) > 0 .AND. i < 0 ) c = c + 1
        END DO
        IF(debug) WRITE(6,'(a,i2)') 'c = ',c ! c=1:7

        ai_data%clock = -1; i = 1
        DO
          ai_data%line_pos = ai_data%clock*i

          IF( ai_data%line_pos > 4 .OR. ai_data%line_pos < -4 ) THEN ! about-face at end of range
            ai_data%clock = -1*ai_data%clock; i = 1; ai_data%line_pos = ai_data%clock*i
          END IF

          IF(debug) WRITE(6,'(a,i2)') 'line_pos2 = ',ai_data%line_pos

          IF( ai_data%line(ai_data%line_pos,1) >= 1 .OR. i == 5 ) EXIT
          i = i + 1
        END DO
        IF(debug) WRITE(6,'(a,i2)') 'i = ',i ! if i=5 then an error has occurred

        IF( ai_data%line( ai_data%line_pos, 1 ) >= 1 ) THEN ! the next valid point has been found

          p0t%x = ai_data%line( ai_data%line_pos, 1 ); p0t%y = ai_data%line( ai_data%line_pos, 2 ) ! fire at current position
          p0t%s = defence(p0t%x,p0t%y)

          CALL display_target( p0t%x,p0t%y )
          IF(delay) CALL SLEEP(1)

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
              IF(delay) CALL SLEEP(1)
              WRITE(6,'(3a)') "I've sunk your ",TRIM( enemy( hit_ship )%n ),"!"
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

! add limited number of shots, regain ammo when an enemy's ship is sunk, and lose ammo when one of your ships is sunk, equal to the length of that ship multiplied by a scale factor
! replace symbols for blank space, miss and hit: "-" -> " ", "M" -> ".", "H"-> "*"

PROGRAM battleships_game

USE battleships_mod

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
!      WRITE(6,'(a)',ADVANCE='no') 'Searching for headers ... '
    READ(u,'(a)',IOSTAT=ios) char_in
    IF(ios /= 0) exit
    !char_in = ADJUSTL(char_in) ! contract to remove spaces?
    CALL strip_spaces( char_in )
!      print '(a)', 'line="'//TRIM(char_in)//'"'

    IF(char_in(1:1) == '[') THEN ! type found
      i = LEN(TRIM(char_in))
      READ(char_in(i:i),'(a1)') char_one
      !WRITE(6,'(a,i0,a)') 'LEN=',i,' last="'//char_one//'"'

      IF(char_one == ']') THEN
        READ(char_in(2:i-1),'(a)') char_temp
!          print '(a)', 'Found header ['//TRIM(char_temp)//']'

        SELECT CASE(char_temp)
          CASE('seed')
            DO ! search for tags
!                WRITE(6,'(a)',ADVANCE='no') ' Searching for tags ... '
              READ(u,'(a)',IOSTAT=ios) char_in
              IF(ios /= 0) exit
!                WRITE(6,*)
              ! search for equals sign
              !print '(a)', '  tagLine="'//TRIM(char_in)//'"'
              i = 1
              DO
                READ(char_in(i:i),'(a1)') char_one
                !WRITE(6,'(a,i0)') 'i=',i
                IF(char_one == '=') EXIT
                IF(i == LEN(TRIM(char_in))) error STOP 'error: could not identify a tag'
                i = i + 1
              END DO
              !WRITE(6,'(a,i0)') ' Found = at i=',i
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
      !  WRITE(6,'(a,i0,a)') 'i=',i,' char_one='//char_one
      !  IF(char_one == ']') EXIT
      !  i = i + 1
      !END DO
      !WRITE(6,'(a,i0)') 'i=',i
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
CALL ranseed(TRIM(seed_operation),'seedbank_'//TRIM(seed_bank)//'.dat') ! saves and loads seeds from file

call sleep(2)
!WRITE(6,'(a)',ADVANCE='no') 'Press enter to continue...'; read(stdin,*)
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

! Write logo
! write(stdout,"(a)", advance="no") CLEAR_SCREEN
! CALL logo

! ! Welcome player
! print '(/,a)', ' NB: This program uses the SLEEP function to add '
! print '(a)', '     delays to the gameplay on purpose.          '
! call sleep(2)
!WRITE(6,'(a)',ADVANCE='no') 'Press enter to continue...'; read(stdin,*)

DO ! Main Menu

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
    WRITE(6,'(a,1x)', ADVANCE='no') '<I> Enter your chosen option from the # column [1-4]:'
    read(stdin,'(i1)', iostat=i) ans_n1
    if(i == iostat_end) stop
    if(i /= 0) cycle
    IF(ans_n1 >= 1 .AND. ans_n1 <= 4) EXIT
  END DO

  SELECT CASE(ans_n1)
    CASE(1)
      DO ! Display ship placement menu
        write(stdout,"(a)", advance="no") CLEAR_SCREEN
        WRITE(6,*)
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

      call sleep(2)

  END SELECT
END DO

IF(two_ai) THEN
  CALL ranship(player1,defence) ! offence
END IF

! debug loop for player2's ships
!DO
  CALL ranship(player2,comp)
  !CALL grid_1(comp,'c')
  !comp = '--'
  !CALL SLEEP(1)
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

    IF( delay ) CALL SLEEP(1)
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
        IF( delay ) CALL SLEEP(1)
        print '(3a)', "You've sunk my ",TRIM( player2( hit_ship )%n ),"!"
        hits(1)%s = hits(1)%s + 1
      END IF

    END IF

  END IF

  !p2sunk = .TRUE.

  IF( ALL( p2sunk ) ) THEN
    IF(delay) CALL SLEEP(1)
    print '(a)', "You've sunk all my ships!"
  END IF

  IF( delay ) CALL SLEEP(1)

  ! Player 2's turn

  print '(/,a)', "Player 2's turn."

  CALL ai( defence, player1, p1sunk, hits, 2, delay, ai2, health )

  !p1sunk = .TRUE.

  IF(ALL(p1sunk)) THEN
    IF(delay) CALL SLEEP(1)
    print '(a)', "I've sunk all your ships!"
  END IF

  IF(.NOT.GET_TO_THE_END) CALL SLEEP(1)
  write(stdout,"(a)", advance="no") CLEAR_SCREEN
  write(stdout,*)
  !CALL grid_1(comp,'c'); write(stdout,*)
  CALL grid_2(defence,offence)
  CALL scoreboard(hits_title,hits)
  CALL scoreboard(health_title,health)

  IF(ALL(p2sunk) .AND. ALL(p1sunk)) THEN

    IF(delay) CALL SLEEP(1)
    print '(a)', "It's a draw!"

  ELSE

    IF(ALL(p2sunk)) THEN
      IF(delay) CALL SLEEP(1)
      print '(a)', 'You win!'
    END IF

    IF(ALL(p1sunk)) THEN
      IF(delay) CALL SLEEP(1)
      print '(a)', 'You lose!'
    END IF

  END IF

  ! Determine if game ends. (all of a player's ships are sunk)
  IF(ALL(p2sunk) .OR. ALL(p1sunk)) EXIT ! exit round loop

  r = r + 1

END DO

IF(delay) CALL SLEEP(1)
print '(a)', 'Thanks for playing!'

END PROGRAM
