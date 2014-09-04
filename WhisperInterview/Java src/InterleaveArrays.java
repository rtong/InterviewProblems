package Whisper;

import java.util.Arrays;

public class InterleaveArrays {
	/**
	 * Basic idea is pretty much like BFS
	 * 1. First thing to do is to get the total number of elements in k arrays
	 * 	  This step takes O(k) time.
	 * 2. Keep traversing arrays level by level
	 * 3. If the level less than the length of current array, 
	 * 	  it indicates that there are no more elements in this array of this level
	 * 4. The loop condition is i < N, which means there still exist elements to search
	 * 	  This step O(N) time, N represents the total number of elements
	 * @param an arbitrary number of arrays of integers
	 * @return a single array of integers which is the interleaving of the argument arrays
	 */
	public int[] interleave(int[][] arrays) {
		// corner case
		if (arrays == null || arrays.length == 0) {
			return new int[0];
		}
		
		// count for the length of merged array in O(k) time
		int len = 0;
		for (int[] ar : arrays) {
			len += ar.length;
		}
		int[] res = new int[len];
		
		// traverse k arrays level by level in O(N) time
		// N is the number of all the integers in k arrays
		int level = 0, i = 0;
		while (i < len) {
			for (int[] ar : arrays) {
				if (level < ar.length) {
					res[i] = ar[level];
					i++;
				}
			}
			level++;
		}
		
		return res;
	}
	
	private void print(int[] ar) {
		System.out.print("[");
		for (int e : ar) {
			System.out.print(e + " ");
		}
		System.out.println("]");
	}
	
	public static void main(String[] args) {
		InterleaveArrays ins = new InterleaveArrays();
		// test case 1 is sample test case
		int[][] TC1 = {{1,2,3}, {4,5}, {6,7,8}};
		// test case 2 is sample test case
		int[][] TC2 = {{1}, {2}, {3}, {4,5,6,7}};
		// test case 3 tests on an array of null
		int[][] TC3 = {};
		// test case 4 tests on an array of empty arrays
		int[][] TC4 = {{}, {}, {}};
		// test case 5 tests on an array of arrays of the same length
		int[][] TC5 = {{1},{2},{3},{4},{5}};
		// test case 6 tests on an array of arrays of the same length except for one
		int[][] TC6 = {{1,2,3,4}, {5}, {6}, {7}};
		// test case 7 tests on an array of rugged arrays with one empty array
		int[][] TC7 = {{1,2,3}, {4}, {}, {5, 6,7,8}};
		// test case 8 tests on an array of empty arrays except for two
		int[][] TC8 = {{1,2,3}, {}, {}, {4, 5,6}};
		// test case 9 tests on an array of empty arrays except for one
		int[][] TC9 = {{}, {}, {1,2,3,4,5,6}, {}, {}};
		// test case 10 tests on rugged arrays
		int[][] TC10 = {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}, {2,2,2,2,2,2,2,2,2,2,2,2}, {3,3,3,3,3,3,3}};
		// test case 11 tests on huge number of data
		int[][] TC11 = new int[3][];
		TC11[0] = new int[1000];
		TC11[1] = new int[500];
		TC11[2] = new int[750];
		Arrays.fill(TC11[0], 1);
		Arrays.fill(TC11[1], 2);
		Arrays.fill(TC11[2], 3);
		// test case 12 tests on big k value and increasing length of arrays
		int[][] TC12 = new int[100][];
		for (int i = 0; i < 100; i++) {
			TC12[i] = new int[i];
			Arrays.fill(TC12[i], i);
		}
		// test case 13 tests on null input
		int[][] TC13 = null;
		
		System.out.print("TC1 Expected: [1 4 6 2 5 7 3 8], Result: ");
		ins.print(ins.interleave(TC1));
		System.out.print("TC2 Expected: [1 2 3 4 5 6 7], Result: ");
		ins.print(ins.interleave(TC2));
		System.out.print("TC3 Expected: [], Result: ");
		ins.print(ins.interleave(TC3));
		System.out.print("TC4 Expected: [], Result: ");
		ins.print(ins.interleave(TC4));
		System.out.print("TC5 Expected: [1 2 3 4 5], Result: ");
		ins.print(ins.interleave(TC5));
		System.out.print("TC6 Expected: [1 5 6 7 2 3 4], Result: ");
		ins.print(ins.interleave(TC6));
		System.out.print("TC7 Expected: [1 4 5 2 6 3 7 8], Result: ");
		ins.print(ins.interleave(TC7));
		System.out.print("TC8 Expected: [1 4 2 5 3 6], Result: ");
		ins.print(ins.interleave(TC8));
		System.out.print("TC9 Expected: [1 2 3 4 5 6], Result: ");
		ins.print(ins.interleave(TC9));
		System.out.print("TC10 Expected: [1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 1 2 1 2 1 2 1 2 1 1 1], Result: ");
		ins.print(ins.interleave(TC10));
		System.out.print("TC11 Expected: [(123)*(13)*1*], Result: ");
		ins.print(ins.interleave(TC11));
		System.out.print("TC12 Expected: [(1, 2, 3, ..., 99), (2, 3, 4, ..., 99), ..., (98, 99), 99], Result: ");
		ins.print(ins.interleave(TC12));
		System.out.print("TC13 Expected: [], Result: ");
		ins.print(ins.interleave(TC13));
	}
}
