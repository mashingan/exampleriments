from std/strutils import parseInt
from std/algorithm import sort, SortOrder
from std/math import sum
from std/sugar import dump

const input {.strdefine.} = "input.txt"

type
  Biggest3Calories = array[0..3, int]
func assignBiggest3Calories(maxc: var Biggest3Calories, id, newcalory: int) =
  if id <= maxc.high:
    maxc[id-1] = newcalory
  else:
    maxc[^1] = newcalory
    sort maxc, SortOrder.Descending
    maxc[^1] = 0

proc countCalories: Biggest3Calories =
  var
    maxcalories = [0, 0, 0, 0]
    elfturn = 1
    currentCalories = 0
  for line in input.lines:
    let calory = try: parseInt(line) except: 0
    if calory == 0:
      assignBiggest3Calories(maxcalories, elfturn, currentCalories)
      currentCalories = 0
    else:
      currentCalories += calory
    inc elfturn

  if currentCalories > 0:
    assignBiggest3Calories(maxcalories, elfturn, currentCalories)
  maxcalories

let maxcalories = countCalories()
echo "part 1, max calorie: ", maxcalories[0]
echo "part 2, sum biggest 3 calories: ", maxcalories.sum
