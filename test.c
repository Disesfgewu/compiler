#include <stdio.h>
#include <stdlib.h>

int main() {
  // 分配空間給一個整數
  int *ptr = (int *)malloc(sizeof(int)); 

  if (ptr == NULL) {
    printf("記憶體分配失敗!\n");
    return 1; 
  }

  *ptr = 10; 
  printf("在記憶體位置 %p 的值是 %d\n", ptr, *ptr);

  // 釋放記憶體
  free(ptr);
  return 0;
}