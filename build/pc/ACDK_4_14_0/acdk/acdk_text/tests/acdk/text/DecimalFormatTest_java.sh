

java -cp . DecimalFormatTest "xxx00.00" 1234
java -cp . DecimalFormatTest "###00.00" 1234456
java -cp . DecimalFormatTest "###,###.00" 1234456
java -cp . DecimalFormatTest "###,###.00" -1234456
java -cp . DecimalFormatTest "###,###.00" NAN
java -cp . DecimalFormatTest "'This is a text'###,###.00" 1234456
java -cp . DecimalFormatTest "%###,###.00" -0.01234456
java -cp . DecimalFormatTest "###,###.00%" -0.01234456
java -cp . DecimalFormatTest "###,###.00%" -0.01234456