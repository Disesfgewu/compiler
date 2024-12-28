#include <stdio.h>
#include <string.h>

int main() {
    const char *str1 = "";  // 第一個字串
    const char *str2 = "a"; // 第二個字串

    int result = strcmp("", "a"); // 使用 strcmp 進行比較

    // 根據 strcmp 的返回值輸出比較結果
    if (result < 0) {
        printf("\"%s\" < \"%s\": True\n", str1, str2);
    } else if (result == 0) {
        printf("\"%s\" == \"%s\": False\n", str1, str2);
    } else {
        printf("\"%s\" > \"%s\": False\n", str1, str2);
    }

    return 0;
}
