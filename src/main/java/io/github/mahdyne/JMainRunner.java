package io.github.mahdyne;


/**
 * @author mahdyne on 10/5/19.
 */
public class JMainRunner {
    public static void main(String[] args) {
        int[] arr= {7, 1, 3, 2, 4, 5, 6};
        System.out.println(minSwaps(arr));
    }
    public static int minSwaps(int[] arr){
        int swaps=0;
        for(int i=0;i<arr.length;i++){
            if(arr[i]!=i+1){
                int indexOfTheRight=indexOfInt(arr,i+1);
                int t=arr[i];
                arr[i]=arr[indexOfTheRight];
                arr[indexOfTheRight]=t;
                swaps++;
            }
        }

        for(int i=0;i<arr.length;i++){
            System.out.println(arr[i]);
        }
        System.out.println();
        return swaps;
    }
    public static int indexOfInt(int[] arr,int key){
        int res=-1;
        for(int i=0;i<arr.length;i++){
            if(arr[i]==key) {
                res = i;
                break;
            }
        }
        return res;
    }
}