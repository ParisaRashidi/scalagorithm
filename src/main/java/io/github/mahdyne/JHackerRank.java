package io.github.mahdyne;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author mahdyne on 12/6/19.
 */
public final class JHackerRank {
    static String minimumBribes(int[] q) {
        int bribe = 0;
        boolean chaotic = false;
        int n = q.length;
        for(int i = 0; i < n; i++)
        {
            if(q[i]-(i+1) > 2)
            {
                chaotic = true;
                break;
            }
            for (int j = Integer.max(0, q[i]-2); j < i; j++)
                if (q[j] > q[i])
                    bribe++;
        }
        if(chaotic)
            return "Too chaotic";
        else
            return Integer.toString(bribe);
    }
    public static int minimumSwap(int arr[]) {
        int swapCounter = 0;
        for (int i = 1; i <= arr.length; i++) {
            if (arr[i - 1] != i) {
                int index = indexOfIntArray(arr,i);
                int tmp = arr[i - 1];
                arr[i - 1] = i;
                arr[index] = tmp;
                swapCounter += 1;
            }
        }
        return swapCounter;
    }
    static long arrayManipulation(int n, int[][] queries) {
        int firstVal=0;
        long[] storage=new long[n];
        long max=queries[0][0];
        for(int i=0;i<queries.length;i++) {
            for (int j = queries[i][0] - 1; j <= queries[i][1] - 1; j++) {
                storage[j] += queries[i][2];
                if(storage[j]>max)
                    max=storage[j];
            }
        }
        return max;
    }
    static long arrayManipulationV2(int n, int[][] queries) {
        int firstVal=0;
        int[][] storage = new int[0][];
        storage[0][0]=1;
        return 1;
    }
    public static int indexOfIntArray(int[] array, int key) {
        int returnvalue = -1;
        for (int i = 0; i < array.length; ++i) {
            if (key == array[i]) {
                returnvalue = i;
                break;
            }
        }
        return returnvalue;
    }
    static String checkMagazine(String[] magazine, String[] note) {
        Map<String,Integer> mmap=new HashMap<String,Integer>();
        String result="";
        for(String str:magazine){
            int v=mmap.getOrDefault(str,0);
            if(v==0)
                mmap.put(str,1);
            else
                mmap.put(str,v+1);
        }
        for(String str:note) {
            int c=mmap.getOrDefault(str,0);
            if(c<1) {
                result="No";
                break;
            }else {
                mmap.put(str,c-1);
            }
        }
        return result;
    }
    static String twoStrings(String s1, String s2) {
        Set<String> set1= Stream.of(s1.split("")).collect(Collectors.toSet());
        Set<String> set2= Stream.of(s2.split("")).collect(Collectors.toSet());
        Set<String> total=Stream.concat(set1.stream(),set2.stream()).collect(Collectors.toSet());
        if(total.size()!=set1.size()+set2.size())
            return "YES";
        else
            return "NO";
    }
}
