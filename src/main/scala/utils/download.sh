#!/bin/bash

DAY=$( date +"%d" | sed 's/^0*//' )
DAY="25"
mkdir -p "/Users/bradley.king/dev/aoc2023/src/main/scala/day$DAY"

#DAY="    "
echo "Pulling input for day $DAY"
OUTPUT="/Users/bradley.king/dev/aoc2023/src/main/scala/day$DAY/in.txt"
echo "Writing to file: $OUTPUT"

URL="https://adventofcode.com/2023/day/$DAY/input"
SESSION="53616c7465645f5f748367e0c6e6cef542e5c7df7f30f613b656408017f7370fa62739d684d930eb95dc2dc7be6f1641071a4b5c5d46118980ae90a19dc71c7b"

curl -A "@bracl via curl" --location --request GET "$URL" --header "Cookie: session=$SESSION" -o "$OUTPUT"
echo "Happy Solving :)"


DESTINATION="/Users/bradley.king/dev/aoc2023/src/main/scala/day$DAY/day$DAY.scala"
cp "/Users/bradley.king/dev/aoc2023/src/main/scala/utils/template" "$DESTINATION"
sed -i.bak s/DAY/$DAY/g "$DESTINATION"
sed -i.bak s/utils$/day$DAY/g "$DESTINATION"
rm $DESTINATION.bak

