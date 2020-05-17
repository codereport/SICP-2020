import java.util.*;

public class Factorial{

    public static int fact(int val) {
        return IntStream.rangeClosed(1, val)
                        .reduce(1, (acc, n) -> acc * n);
    }

    public static void main(String []args){
        System.out.println(fact(10));
    }
}
