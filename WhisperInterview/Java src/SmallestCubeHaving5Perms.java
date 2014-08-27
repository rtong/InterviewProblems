package Whisper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

public class SmallestCubeHaving5Perms {
	public long solution() {
		HashMap<Long, List<Long>> map = new HashMap<Long, List<Long>>();
		long res = 0;
		
		long i = 1;
		while (true) {
			long num = i * i * i;
			long maxNum = getMaxPermutation(num);
			if (!map.containsKey(maxNum)) {
				List<Long> list = new ArrayList<Long>();
				list.add(num);
				map.put(maxNum, list);
			} else {
				map.get(maxNum).add(num);
				if (map.get(maxNum).size() == 5) {
					res = map.get(maxNum).get(0);
					for (Long l : map.get(maxNum)) {
						System.out.print(l + ", ");
					}
					System.out.println();
					break;
				}
			}
			i++;
		}
		
		return res;
	}
	private long getMaxPermutation(long num) {
		List<Integer> digits = new ArrayList<Integer>();
		while (num != 0) {
			digits.add((int)(num % 10));
			num = num / 10;
		}
		Collections.sort(digits);
		for (int i = digits.size() - 1; i >= 0; i--) {
			num = num * 10 + digits.get(i);
		}
		return num;
	}
	
	public long isCube(long num) {
		long start = 1;
		long end = (long)Math.sqrt(num);
		while (start + 1 < end) {
			long mid = start + (end - start) / 2;
			long cube = mid * mid * mid;
			if (cube == num) {
				return mid;
			} else if (cube < num) {
				start = mid;
			} else if (cube > num) {
				end = mid;
			}
		}
		if (start * start * start == num) {
			return start;
		}
		if (end * end * end == num) {
			return end;
		}
		return -1;
	}
	
	public static void main(String[] args) {
		SmallestCubeHaving5Perms ins = new SmallestCubeHaving5Perms();
		System.out.println("Smallest one is: " + ins.solution());
		System.out.println(ins.isCube(127035954683l));
	}
}
