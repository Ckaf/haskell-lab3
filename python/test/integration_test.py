
import pytest
import os


def test_1():
    stream = os.popen('cat ./python/test/data/points.txt | haskell-lab3-exe --left 1 --right 10 --lm')
    res = stream.read()
    file = open("./python/test/data/expected_result1.txt", "r")
    expected = file.read()
    assert(res == expected)

def test_2():
    stream = os.popen('cat ./python/test/data/points.txt | haskell-lab3-exe --left 1 --right 10 --sm')
    res = stream.read()
    file = open("./python/test/data/expected_result2.txt", "r")
    expected = file.read()
    assert(res == expected)

def test_3():
    stream = os.popen('cat ./python/test/data/points.txt | haskell-lab3-exe --left 1 --right 10 --lm --sm')
    res = stream.read()
    file = open("./python/test/data/expected_result3.txt", "r")
    expected = file.read()
    assert(res == expected)

def test_4():
    stream = os.popen('haskell-lab3-exe --left 1 --right 10 --sm --file \"./data/points\"')
    res = stream.read()
    file = open("./python/test/data/expected_result4.txt", "r")
    expected = file.read()
    assert(res == expected)
