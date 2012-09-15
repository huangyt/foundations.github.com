import java.util.*;
import java.lang.*;
public class CalendarTest
{
public static void main(String[] args)
{
  
  if (args.length < 3) {
    System.out.println("year month mday [hour minute second]");
    return;
  }
  
  int y = Integer.parseInt(args[0]);
  int m = Integer.parseInt(args[1]);
  int md = Integer.parseInt(args[2]);
  int h = 0;
  int min = 0;
  int sec = 0;
  
 Calendar calendar = new GregorianCalendar();
 
 Date trialTime = new Date(y - 1900, m, md, h, min, sec);
 calendar.setTime(trialTime);
 dumpCalendar(calendar);
 }
  static void dumpCalendar(Calendar calendar)
  {
    
 // print out a bunch of interesting things
 System.out.println("time in ms: "  + calendar.getTime().getTime());
 System.out.println("ZONE-ID:              " + calendar.getTimeZone().getID());
 System.out.println("ERA: "  + calendar.get(Calendar.ERA));
 System.out.println("YEAR: "  + calendar.get(Calendar.YEAR));
 System.out.println("MONTH: "  + calendar.get(Calendar.MONTH));
 System.out.println("WEEK_OF_YEAR: "  + calendar.get(Calendar.WEEK_OF_YEAR));
 System.out.println("WEEK_OF_MONTH: "  + calendar.get(Calendar.WEEK_OF_MONTH));
 System.out.println("DATE: "  + calendar.get(Calendar.DATE));
 System.out.println("DAY_OF_MONTH: "  + calendar.get(Calendar.DAY_OF_MONTH));
 System.out.println("DAY_OF_YEAR: "  + calendar.get(Calendar.DAY_OF_YEAR));
 System.out.println("DAY_OF_WEEK: "  + calendar.get(Calendar.DAY_OF_WEEK));
 System.out.println("DAY_OF_WEEK_IN_MONTH: "
                     + calendar.get(Calendar.DAY_OF_WEEK_IN_MONTH));
 System.out.println("AM_PM: "  + calendar.get(Calendar.AM_PM));
 System.out.println("HOUR: "  + calendar.get(Calendar.HOUR));
 System.out.println("HOUR_OF_DAY: "  + calendar.get(Calendar.HOUR_OF_DAY));
 System.out.println("MINUTE: "  + calendar.get(Calendar.MINUTE));
 System.out.println("SECOND: "  + calendar.get(Calendar.SECOND));
 System.out.println("MILLISECOND: "  + calendar.get(Calendar.MILLISECOND));
 System.out.println("ZONE_OFFSET: "
                     +  (calendar.get(Calendar.ZONE_OFFSET)/(60*60*1000)));
 System.out.println("DST_OFFSET: " + 
                     (calendar.get(Calendar.DST_OFFSET)/(60*60*1000)));
/*
 System.out.println("Current Time, with hour reset to 3");
 calendar.clear(Calendar.HOUR_OF_DAY); // so doesn't override
 calendar.set(Calendar.HOUR, 3);
 System.out.println("ERA: "  + calendar.get(Calendar.ERA));
 System.out.println("YEAR: "  + calendar.get(Calendar.YEAR));
 System.out.println("MONTH: "  + calendar.get(Calendar.MONTH));
 System.out.println("WEEK_OF_YEAR: "  + calendar.get(Calendar.WEEK_OF_YEAR));
 System.out.println("WEEK_OF_MONTH: "  + calendar.get(Calendar.WEEK_OF_MONTH));
 System.out.println("DATE: "  + calendar.get(Calendar.DATE));
 System.out.println("DAY_OF_MONTH: "  + calendar.get(Calendar.DAY_OF_MONTH));
 System.out.println("DAY_OF_YEAR: "  + calendar.get(Calendar.DAY_OF_YEAR));
 System.out.println("DAY_OF_WEEK: "  + calendar.get(Calendar.DAY_OF_WEEK));
 System.out.println("DAY_OF_WEEK_IN_MONTH: "
                     + calendar.get(Calendar.DAY_OF_WEEK_IN_MONTH));
 System.out.println("AM_PM: "  + calendar.get(Calendar.AM_PM));
 System.out.println("HOUR: "  + calendar.get(Calendar.HOUR));
 System.out.println("HOUR_OF_DAY: "  + calendar.get(Calendar.HOUR_OF_DAY));
 System.out.println("MINUTE: "  + calendar.get(Calendar.MINUTE));
 System.out.println("SECOND: "  + calendar.get(Calendar.SECOND));
 System.out.println("MILLISECOND: "  + calendar.get(Calendar.MILLISECOND));
 System.out.println("ZONE_OFFSET: "
         + (calendar.get(Calendar.ZONE_OFFSET)/(60*60*1000))); // in hours
 System.out.println("DST_OFFSET: "
         + (calendar.get(Calendar.DST_OFFSET)/(60*60*1000))); // in hours
 */
 }
 }