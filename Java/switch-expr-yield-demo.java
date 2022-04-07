class Demonstration {
  public static boolean tryIt(String input) {
    int value = switch (input) {
      case "Case 1" -> 1;
      case "Case 2" -> 2;
      case "Case 1 & Print" -> {
        System.out.println("Case 1");
        yield 2;
      }
      case "Case 2 & Print" -> {
        System.out.println("Case 2");
        yield 3;
      }
      default -> 4;
    };
    System.out.println(value);
    return value % 2 == 0;
  }
  // This is equivalent to:
  public static boolean equiv(String input) {
    int value;
    switch (input) {
      case "Case 1":
        value = 1;
        break;
      case "Case 1 & Print":
        System.out.println("Case 1");
      case "Case 2":
        value = 2;
        break;
      case "Case 2 & Print":
        System.out.println("Case 2");
        value = 3;
        break;
      default:
        value = 4;
    }
    System.out.println(value);
    return value % 2 == 0;
  }
}
