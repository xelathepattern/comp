#include <stdio.h>

void int_to_str(int n, char *str) {
    int i = 0;
    int negative;
    if (n < 0) {
        negative = 1;
        n = -n;
    }
    else {
        negative = 0;
    }
    // makes string in backwards order
    if (n == 0) {
        str[0] = '0';
        str[1] = '\0';
        return;
    }

    while (n > 0) {
        str[i+negative] = n % 10 + '0'; // addition of ascii codes
        n /= 10;

        i = i+1;
    }
    str[i+negative] =  '\0'; // null terminate
    
    // reverse string
    for (int j=0, k=i-1; j<k; j++, k--) {
        char temp = str[j+negative];
        str[j+negative] = str[k+negative];
        str[k+negative] = temp;
    }
    
    if (negative==1) {
        str[0] = '-';
    }
}

int int_log_10(int n) {
    int i = 0;
    while (n > 0) {
        n /= 10;
        i = i+1;
    }

    return i;
}

void print_arr(int arr[], int n) {        
    for (int i=0; i<n; i++) {
        int int_to_print = arr[i];
        int negative;
        if (int_to_print<0) {
            negative = 1;
        }
        else {
            negative = 0;
        }

        int str_length = int_log_10(int_to_print) + negative;
        char str[str_length];
        int_to_str(int_to_print, str);
        printf("%s, ", str);
    }
    printf("\n");
}

int fact(int n) {
    int acc = 1;
    for (int i=1; i<=n; i++) {
        acc *= i;
    }

    return acc;
}

void next_perm(int this_perm[], int dirs[], int n) { 
    int largest_mobile_indice = 0;
    int largest_mobile_int = 0;
    for (int i=0; i<n; i++) {
        int this_int = this_perm[i];
        
        if (i + dirs[i] >= n) {
            continue;
        }
        if (i + dirs[i] < 0) {
            continue;
        }

        if (this_int > this_perm[i + dirs[i]]) {
            if (largest_mobile_int < this_int) {
                largest_mobile_int = this_int;
                largest_mobile_indice = i;
            }
        }
    }
    char str_to_p[int_log_10(largest_mobile_int)];
    int_to_str(largest_mobile_int, str_to_p);
    // printf("largest_mobile_int = %s \n", str_to_p);

    int swap1_indice = largest_mobile_indice;
    int swap_dir = dirs[largest_mobile_indice];
    int swap2_indice = largest_mobile_indice + swap_dir;
    int _temp_dir = dirs[swap1_indice];
    
    this_perm[swap1_indice] = this_perm[swap2_indice];
    dirs[swap1_indice] = dirs[swap2_indice];
    this_perm[swap2_indice] = largest_mobile_int;
    if (swap2_indice > 0) {
        dirs[swap2_indice] = _temp_dir;
    }
    else {
        dirs[0] = 0;
    }

    int move_lower_towards_indice = swap2_indice;
    for (int i=0; i<n; i++) {
        int this_int = this_perm[i];
        if (this_int > largest_mobile_int) {
            if (i > move_lower_towards_indice) {
                dirs[i] = -1;
            }
            else if (i < move_lower_towards_indice) {
                dirs[i] = +1;
            }
        }
    }
}

int main() {
    int n = 4;
    int perm[n];
    int dirs[n];

    dirs[0] = 0;
    for (int i=0; i<n; i++) {
        perm[i] = i+1;
        if (i>0) {
            dirs[i] = -1;
        }
    } 
    print_arr(perm, n);
    print_arr(dirs, n);

    int num_perms = fact(n);
    for (int i=1; i<num_perms; i++) {
        next_perm(perm, dirs, n);
        print_arr(perm, n); 
        //print_arr(dirs, n);
    }

    return 0;
}
