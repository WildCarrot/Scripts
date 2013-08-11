#!/usr/bin/python

import sys,unittest,string

sys.path.append("..")

import mkpasswd
import random

class TestLength(unittest.TestCase):

    def test_default_length(self):
        s = mkpasswd.PasswordString()
        self.assertEqual(len(s), 8)

    def test_change_length(self):
        exp_len = random.randint(0, 100)
        s = mkpasswd.PasswordString(length=exp_len)
        self.assertEqual(len(s), exp_len)

    def test_change_lower(self):
        exp_lower = random.randint(0, 50)
        s = mkpasswd.PasswordString(length=80, lower=exp_lower)
        act_lower = 0
        for c in list(str(s)):
            if c in string.ascii_lowercase:
                act_lower += 1
        self.assertGreaterEqual(act_lower, exp_lower)


if __name__ == '__main__':
    unittest.main()

