# def str(i):
#     l = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
#     if i < 10:
#         return l[i]
#     else:
#         return str(i // 10) + l[i % 10]

# print(str(42))


# zéro, une ou plusieurs définitions de fonctions au début du fichier
def fibaux(a, b, k):
    if k == 0:
        return a
    else:
        return fibaux(b, a+b, k-1)

def fib(n):
    return fibaux(0, 1, n)

# une ou plusieurs instructions à la fin du fichier
print("quelques valeurs de la suite de Fibonacci :")
for n in [0, 1, 11, 42]:
    print(fib(n))

