#!/usr/bin/python
#
# TODO
# - command line
# - alternate left/right?
# - favor letters over symbols for extras
# - words

import random
import sys
import string

class PasswordPhrase:
    def __init__(self, num_words=4, wordlen_min=4, wordlen_max=10, seed=37):
        self.num_words = num_words
        if wordlen_max < wordlen_min:
            raise ValueError("Given word length max (%) is less than given word len min (%)." % [wordlen_max, wordlen_min])
        self.wordlen_min = wordlen_min
        self.wordlen_max = wordlen_max
        self.value = None
        if seed != 37:
            random.seed(seed)
        self.generate()

    def generate(self):
        s = []

        f = open("/usr/share/dict/words", "r")
        words = f.readlines()
        f.close()

        # Get words out of the word list and check them for length.
        # Don't get carried away if the length is impossible to find.
        i = 0
        while len(s) < self.num_words and i < 10000:
            w = random.choice(words)[0:-1] # remove newline
            #if len(w) > self.wordlen_min and len(w) < self.wordlen_max:
            if len(w) == self.wordlen_min or len(w) == self.wordlen_max:
                s.append(w)
            i += 1

        if len(s) != self.num_words:
            raise ValueError("Could not find enough words matching given criteria.")

        self.value = " ".join(s)

    def __len__(self):
        return len(self.value)

    def __str__(self):
        return self.value


class PasswordString:
    def __init__(self, length=8, lower=2, upper=2, symbols=2, seed=37):
        self.length = length
        self.minimums = {"lower": {"val":lower, "set":string.ascii_lowercase},
                         "upper": {"val":upper, "set":string.ascii_uppercase},
                         "symbols": {"val":symbols, "set":string.punctuation}}
        if length < (lower + upper + symbols):
            raise ValueError("Given length (%) cannot include requested lower (%), upper (%), and special (%) characters." % [lower, upper, symbols])
        self.alternate = False # Alternate hands?
        self.value = None
        if seed != 37:
            random.seed(seed)
        self.generate()

    def generate(self):
        s = []

        # Get the required special values first.
        for k in self.minimums.keys():
            for n in range(0, self.minimums[k]["val"]):
                s += [random.choice(self.minimums[k]["set"])]

        # Pad out the string with other values.
        for i in range(len(s), self.length):
            n = random.choice(self.minimums.keys())
            s += [random.choice(self.minimums[n]["set"])]

        random.shuffle(s)
        self.value = "".join(s)

    def __len__(self):
        return len(self.value)

    def __str__(self):
        return self.value


def genpasswd():
    # parse command line
    s = PasswordString()
    print s

    w = PasswordPhrase()
    print w

if __name__ == '__main__':
    genpasswd()
