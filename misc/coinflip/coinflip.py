from random import randint

def flip():
    return randint(0,1)

def binary_choice(p_bits, given_fail_p=False, ret_n=False): # p_bits: Int -> Bit
    # Tails: Same bit as p 
    # Heads: Diff bit 
    # When it's diff, we are greater iff the p_bit is 0
    n = 0
    while True:
        if flip():
            choice = p_bits(n)
            if given_fail_p:
                choice = (choice + 1) % 2 # complement of choice, because if p is fail prob then left (lower) box is fail and right (greater) box is succ.
            if ret_n:
                return (choice, n)
            else:
                return choice
        else:
            n += 1

def nary_choice(psums, ret_n=False): # psums: [Int -> Bit] are the partial sums of the choice probabilities. Alternatively, psums[i] represents P(choice <= i)
    # Tails: 0 
    # Heads: 1
    # When it's diff from p, we are greater than p iff the p_bit is 1
    n = 0 
    low_consider_index = 0
    high_consider_index = len(psums) - 1
    while low_consider_index <= high_consider_index:
        flipped = flip()
        for i in range(low_consider_index, high_consider_index + 1):
            #each time we are greater than something, elim it (and all before) 
            #the first time we are smaller than something, elim it and all after
            this_psum_digit = psums[i](n)
            if flipped == this_psum_digit:
                if this_psum_digit:
                    # we are smaller 
                    high_consider_index = i - 1
                    break # we'll also be smaller than anything after this.
                else:
                    #we are bigger
                    low_consider_index = i + 1
         
        n += 1

    # at this point low_consider_index < high_consider_index
    # so greater than (low - 1), lower than (high + 1), which leaves just the interval between (low - 1) and (high + 1). This is the interval of index low.
    # |--|--|--|--|--|
    #  --0--1--2--3--
    # with low=2, high=1 we have 
    #  ______--______
    # which is the third (index 2) interval
    return low_consider_index

def counts(arr):
    out = {}
    for x in arr:
        if x in out.keys():
            out[x] += 1
        else:
            out[x] = 1
    return out


def test():
    one_third = lambda n: n%2
    two_third = lambda n: (n+1)%2

    one_fourth = lambda n: (n==1)
    one_half = lambda n: (n==0)

    x = [nary_choice([one_fourth, one_half]) for _ in range(int(1e3))]
    return counts(x)

