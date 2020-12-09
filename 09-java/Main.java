import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Main {
  public static ArrayList<BigInteger> parseFile(String inputFile)
      throws IOException {
    ArrayList<BigInteger> nums = new ArrayList<BigInteger>();
    BufferedReader br = new BufferedReader(new FileReader(new File(inputFile)));
    String line;
    while ((line = br.readLine()) != null) {
      nums.add(new BigInteger(line));
    }
    br.close();
    return nums;
  }

  public static BigInteger part1(ArrayList<BigInteger> data,
                                 int preambleLength) {
    for (int n = preambleLength; n < data.size(); n++) {
      boolean found = false;
      for (int i = n - preambleLength; i < n; i++) {
        for (int j = n - preambleLength; j < n; j++) {
          if (i != j && !found) {
            found = data.get(n).equals(data.get(i).add(data.get(j)));
          }
        }
      }
      if (!found) return data.get(n);
    }
    return null;
  }

  public static BigInteger part2(ArrayList<BigInteger> data,
                                 BigInteger target) {
    for (int i = 0; i < data.size(); i++) {
      BigInteger sum = data.get(i);
      for (int j = i + 1; j < data.size(); j++) {
        sum = sum.add(data.get(j));
        // if the sum matches the answer from part 1
        if (sum.equals(target)) {
          // find min/max in sub array
          List<BigInteger> contigious = data.subList(i, j + 1);
          return Collections.min(contigious).add(Collections.max(contigious));
        } else if (sum.compareTo(target) > 0) {
          // if the contigious sum is larger than target, check next number
          break;
        }
      }
    }
    return null;
  }

  public static void main(String[] args) throws IOException {
    ArrayList<BigInteger> nums = parseFile(args[0]);
    BigInteger part1Solution = part1(nums, 25);
    System.out.printf("Part 1: %s\n", part1Solution);
    System.out.printf("Part 2: %s\n", part2(nums, part1Solution));
  }
}
