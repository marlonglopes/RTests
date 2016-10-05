[abc]	A single character of: a, b, or c
[^abc]	Any single character except: a, b, or c
[a-z]	Any single character in the range a-z
[a-zA-Z]	Any single character in the range a-z or A-Z
^	Start of line
$	End of line
\A	Start of string
\z	End of string
.	Any single character
\s	Any whitespace character
\S	Any non-whitespace character
\d	Any digit
\D	Any non-digit
\w	Any word character (letter, number, underscore)
\W	Any non-word character
\b	Any word boundary
(...)	Capture everything enclosed
(a|b)	a or b
a?	Zero or one of a
a*	Zero or more of a
a+	One or more of a
a{3}	Exactly 3 of a
a{3,}	3 or more of a
a{3,6}	Between 3 and 6 of a

#123.125.528.256
#asd.asd.asd.asd
#123456789.2.2.2
#123.125.528.256
#123.123.123.132.123.123
#...............
#...
#...............................

Regex_Pattern = '(.{3,}\..{3,}\..{3,})'

Test_String = gets #(nil)
regex = Test_String.scan /#{Regex_Pattern}/
print "Number of matches : ", regex.length