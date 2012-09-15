

print("\nPerl World\n\n");


$sb = acdk::new("acdk/lang/StringBuffer", "Hi ");
$sb->append("ACDK World");
print($sb->toString() . "\n");;
exit 0;
