#!/bin/bash

DAY=$( date +"%d" | sed 's/^0*//' )
mkdir -p "/Users/bradley.king/dev/aoc2023/src/main/scala/day$DAY"

#DAY="    "
echo "Pulling input for day $DAY"
OUTPUT="/Users/bradley.king/dev/aoc2023/src/main/scala/day$DAY/in.txt"
echo "Writing to file: $OUTPUT"

URL="https://adventofcode.com/2023/day/$DAY/input"
SESSION="53616c7465645f5f076f98f2d6ed226b0527c5232949f8a2fb1d5970ba44694a98b410d2414003635cb664b9777868b4fa0d358860e613b21bda557bea1ca935"

curl -A "@bracl via curl" --location --request GET "$URL" --header "Cookie: session=$SESSION" -o "$OUTPUT"
echo "Happy Solving :)"


DESTINATION="/Users/bradley.king/dev/aoc2023/src/main/scala/day$DAY/day$DAY.scala"
cp "/Users/bradley.king/dev/aoc2023/src/main/scala/utils/template" "$DESTINATION"
sed -i.bak s/DAY/$DAY/g "$DESTINATION"
sed -i.bak s/utils$/day$DAY/g "$DESTINATION"
rm $DESTINATION.bak

