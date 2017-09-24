# Question:
# You are given an integer N and an integer M.
# You are supposed to write a method void findBestCoinsThatMinimizeAverage(int N, int M)
# that prints the best collection of N coins that minimize the average number of minimum
# coins needed to generate values from 1 to M.
# So, if M = 100, and N = 4, then if we use the set {1, 5, 10, 25} to generate each value from 1 to 100,
# so that for each value the number of coins are minimized,
# i.e. 1 = 1 (1 coin), 2 = 1 + 1 (2 coins),...,
# 6 = 1 + 5 (2 coins), ...,
# 24 = 5 + 5 + 5 + 5 + 1 + 1 + 1 + 1 (8 coins),
# and we take the average of these coins, we would see that the average comes out to ~5.7.
# But if we instead use {1, 5, 18, 25}, the average would come out to be 3.7.
# We are to find that set of N coins, and print them, that produce the minimum average.
