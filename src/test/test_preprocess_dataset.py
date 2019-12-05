#!/usr/bin/python
""" test_preprocess_dataset

Author: datadonk23
Date: 29.09.19
"""

import unittest

from nlp.preprocess_dataset import clean_text


class TestCleanText(unittest.TestCase):
    def test_lowercasing(self):
        txt = "Test Text"
        cleaned_txt = clean_text(txt)
        self.assertTrue(cleaned_txt.islower(), "Text not lowercased.")

    def test_punctuation_removal(self):
        txt = "String, contains punctuations. And a german quote: „Hallo”."
        expect_txt = "string contains punctuations and a german quote hallo"
        cleaned_txt = clean_text(txt)
        self.assertEqual(cleaned_txt, expect_txt,
                         "Incorrect punct removal from text.")

    def test_placeholders(self):
        num_txt = "23 - Nichts ist so wie es scheint."
        expect_num_txt = "NUM  nichts ist so wie es scheint"
        cleaned_txt = clean_text(num_txt)
        self.assertEqual(expect_num_txt, cleaned_txt,
                         "Incorrect placeholder for numbers.")

        score_txt = "SKV 2:0 PASK, Halbzeit 1-0."
        expect_score_txt = "skv GAMESCORE  pask halbzeit GAMESCORE "
        cleaned_txt = clean_text(score_txt)
        self.assertEqual(expect_score_txt, cleaned_txt,
                         "Incorrect placeholder for game scores.")


if __name__ == '__main__':
    unittest.main()
