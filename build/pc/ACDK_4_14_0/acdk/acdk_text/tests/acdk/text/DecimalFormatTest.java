import java.text.*;

public class DecimalFormatTest {

public static void main(String[] args)
{
    if (args.length < 2) {
        System.out.println("DecimalFormatTest 'pattern' 'value'");
        return;
    }
    String pattern = args[0];
    String value = args[1];
    if (value.equals("NAN") == true) {
        double f = Double.NaN;
        DecimalFormat df = new DecimalFormat();
        StringBuffer erg = new StringBuffer("");
        df.applyPattern(pattern);
        erg = df.format(f, erg, new FieldPosition(0));
        System.out.println(args[1] + "] + [" + pattern + "] -> [" + erg.toString() + "]");
    } else if (value.indexOf('.') != -1) {
        double f = Double.parseDouble(value);
        DecimalFormat df = new DecimalFormat();
        StringBuffer erg = new StringBuffer("");
        df.applyPattern(pattern);
        erg = df.format(f, erg, new FieldPosition(0));
        System.out.println(args[1] + "] + [" + pattern + "] -> [" + erg.toString() + "]");
    } else {
        long f = Long.parseLong(value);
        DecimalFormat df = new DecimalFormat();
        StringBuffer erg = new StringBuffer("");
        df.applyPattern(pattern);
        erg = df.format(f, erg, new FieldPosition(0));
        System.out.println(args[1] + "] + [" + pattern + "] -> [" + erg.toString() + "]");
    }
  }
}
