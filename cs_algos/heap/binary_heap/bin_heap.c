void up_bin_heap(int heap[], int index_to_upheap);

void bin_heap(int ints[], int size) {
    for (int i=0; i<size; i++) {
        up_bin_heap(ints, i);
    }
}

//for the following: note that if we started indexing from 1, then the last bit
//would tell us whether we were the left or right child, and the first n-1 would
//tell us the index of the parent.
//thus with 0 indexing, to get the parent we do ((index+1)>>1) - 1
//and ((index+1)<<1)-1; ((index+1)<<1); to get the left and right children
void up_bin_heap(int heap[], int index_to_upheap) {
    int parent_index = index_to_upheap >> 1;
    int current_index = index_to_upheap; 
    int current = heap[current_index];
    int parent = heap[parent_index];
    while (current > parent) {
        heap[current_index] = parent;
        heap[parent_index] = current;
        
        current_index = parent_index;
        parent_index = ((current_index+1) >> 1)-1;
        
        current = heap[current_index];
        parent = heap[parent_index];
    } 
}

void down_bin_heap(int heap[], int size, int index_to_downheap) {
    int current_index = index_to_downheap;
    int left_index;
    int right_index;

    int current;
    int left;
    int right;
   
    while (1) {
        int current = heap[current_index];
      
        int left_index = ((current_index+1) << 1)-1;
        if (left_index >= size) {
            break;
        }
        int left = heap[left_index];

        int right_index = (current_index+1) << 1; 
        if (right_index >= size) {
            if (current < left) {
                //swap with left
                heap[left_index] = current;
                heap[current_index] = left;
                current_index = left_index;
                break;
            }
            else {
                break;
            }
        }
        int right = heap[right_index];
         
        if (current < left) {
            if (right > left) {
                //swap with right
                heap[right_index] = current;
                heap[current_index] = right;
                current_index = right_index;
            }
            else {
                //swap with left
                heap[left_index] = current;
                heap[current_index] = left;
                current_index = left_index;
            }
        }
        else if (current < right) {
            //swap with right
            heap[right_index] = current;
            heap[current_index] = right;
            current_index = right_index;
        }
        else {
            break;
        }
    }
}


