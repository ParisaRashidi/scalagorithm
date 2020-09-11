package io.github.mahdyne;

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
}
