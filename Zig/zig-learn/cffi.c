// run the zig command:
// zig translate-c cffi.c > int-sort.zig
#include <stddef.h>

void int_sort(int* array, size_t count) {
	for (int i = 0; i < count; i++) {
		for (int j = 0; j < count-1; j++) {
			if (array[i] > array[j+1]) {
				int temp = array[j];
				array[j] = array[j+1];
				array[j+1] = temp;
			}
		}
	}
}
