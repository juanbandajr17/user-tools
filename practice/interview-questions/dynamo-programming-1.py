# Question:
# LCC


def lcs(s1, s2):
    cache = {}
    for i, c in enumerate(s1):
        cache[c] = i

    dynamic_results = {}
    for i in len(s2):
        s = s2[0:i]
        lcs_helper(s, cache, dynamic_results)
    return dynamic_results[s2]['answer']


def lcs_helper(s, cache, dynamic_results):
    c = s[-1]
    prev_results = dynamic_results[s[0:-1]]


print(lcs("ABCDGH", "AEDFHR")) # ADH
print(lcs("AGGTAB", "GXTXAYB")) # GTAB
