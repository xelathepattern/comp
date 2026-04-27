#include "../../heap/binary_heap/bin_heap.c"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void sort(int ints[], int size);
void linspace(float start, float end, int N, float result[]);
void random_ints(int num_ints, int min, int max, int result[]);


int main() {
    int start_log_test_size = 1;
    int end_log_test_size = 3;
    int num_tests = 100;
    
    int min_int = 0;
    int max_int = 1000000;
    float float_sizes[num_tests];
    linspace(start_log_test_size, end_log_test_size, num_tests, float_sizes);
    int sizes[num_tests];
    for (int i=0; i<num_tests; i++) {
        sizes[i] = (int) float_sizes;
    }

    // implement averaging multiple runs per test (with different random ints)
    
    FILE *fptr;
    for (int i=0; i<num_tests; i++) {
        int ints[sizes[i]];
        random_ints(sizes[i], min_int, max_int, ints);
    
        time_t tic = time(NULL);
        sort(ints, sizes[i]);
        time_t toc = time(NULL);
        // write to file
        fptr = fopen("data.dat", "a");
        fprintf(fptr, "%d, %jd");
         
    }
    fclose(fptr);

    /*
    for (int i=0; i<size; i++) {
        printf("%d\n", ints[i]);
    }
    */

}

//from https://stackoverflow.com/questions/2509679/how-to-generate-a-random-integer-number-from-within-a-range
// Assumes 0 <= max <= RAND_MAX
// Returns in the closed interval [0, max]
long random_at_most(long max) {
    unsigned long
        //max <= RAND_MAX < ULONG_MAX, so this is okay.
        num_bins = (unsigned long) max + 1,
        num_rand = (unsigned long) RAND_MAX + 1,
        bin_size = num_rand / num_bins,
        defect = num_rand % num_bins;

    long x;
    do {
        x = random();
    }
    // This is carefully written not to overflow
    while (num_rand - defect <= (unsigned long)x);

    // Truncated division is intentional
    return x/bin_size;
}

long rand_in_range(long min, long max) {
    return min + random_at_most(max - min);
}

void random_ints(int num_ints, int min, int max, int result[]) {
    for (int i=0; i<num_ints; i++) {
        result[i] = (int) rand_in_range((long) min, (long) max); 
    }
}

void linspace(float start, float end, int N, float result[]) {
    float spacing = (end - start)/N;
    result[0] = start;
    int i = 0;
    while (result[i] < end-spacing) {
       i++;
       if (i >= N) {
           //shouldn't happen, but just in case
           printf("oh no! more than N spacings!\n");
           break;
       }
       result[i] = result[i-1] + spacing;
    }
}

void sort(int ints[], int size) {
    int unsorted_size = size; 
    bin_heap(ints, unsorted_size);

    int tmp;
    while (unsorted_size>0) {
        tmp = ints[0];
        ints[0] = ints[unsorted_size-1];
        ints[unsorted_size-1] = tmp;
        unsorted_size -= 1;
  
        if (unsorted_size == 0) {
            break;
        }
        down_bin_heap(ints, unsorted_size, 0);  
        
    
    }
}
