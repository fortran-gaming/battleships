MODULE misc_mod

use, intrinsic :: iso_c_binding, only : C_INT
use, intrinsic :: iso_fortran_env, only : stdin => input_unit

implicit none

interface
subroutine sleep(ms) bind(C, name="c_sleep")
import
integer(C_INT), intent(in) :: ms
end subroutine
end interface

CONTAINS

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
  !print *, 'fmt = ', fmt_char
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
