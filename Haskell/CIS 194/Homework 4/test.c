#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void sieveOfSundaram(int n) {
    // Adjust n for the algorithm
    int new_n = (n - 2) / 2;

    // Create an array to mark numbers
    bool* marked = (bool*)malloc((new_n + 1) * sizeof(bool));
    for (int i = 0; i <= new_n; ++i) {
        marked[i] = false;
    }

    // Mark numbers according to the Sieve of Sundaram
    for (int i = 1; i <= new_n; ++i) {
        for (int j = i; (i + j + 2 * i * j) <= new_n; ++j) {
            marked[i + j + 2 * i * j] = true;
        }
    }

    // 2 is the only even prime number
    if (n > 2) {
        printf("2 ");
    }

    // Print primes using the marked array
    for (int i = 1; i <= new_n; ++i) {
        if (!marked[i]) {
            printf("%d ", 2 * i + 1);
        }
    }

    printf("\n");
    free(marked);
}

int main(int argc, char *argv[]) {
    int n;
    sieveOfSundaram(atoi(argv[1]));

    return 0;
}
