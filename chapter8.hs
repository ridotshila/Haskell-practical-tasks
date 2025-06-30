-- HC8 - Haskell Chapter 8: Creating Non-Parameterized Types

-- Task 1: Type synonyms and transaction generator
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to value = "From: " ++ from ++ ", To: " ++ to ++ ", Value: " ++ show value

-- Task 2: PaymentMethod and Person
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show
data Person2 = Person2 { pname :: String, paddress :: (String, Int), payment :: PaymentMethod } deriving Show

bob :: Person2
bob = Person2 "Bob" ("123 Street", 1000) Cash

-- Task 3: Shape and area calculation
data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- Task 4: Employee with record syntax
data Employee = Employee { name :: String, experienceInYears :: Float } deriving Show

richard :: Employee
richard = Employee "Richard" 7.5

-- Task 5: Person with employment status
data Person = Person { personName :: String, age :: Int, isEmployed :: Bool } deriving Show

person1, person2 :: Person
person1 = Person "Alice" 30 True
person2 = Person "Charlie" 22 False

-- Task 6: Circle and Rectangle with record-based Shape (new type)
data CircleShape = CircleShape { center :: (Float, Float), radius :: Float, color :: String } deriving Show
data RectangleShape = RectangleShape { width :: Float, height :: Float, colorR :: String } deriving Show

myCircle :: CircleShape
myCircle = CircleShape (0.0, 0.0) 10.0 "Red"

myRectangle :: RectangleShape
myRectangle = RectangleShape 10.0 5.0 "Blue"

-- Task 7: Animal description
data Animal = Dog String | Cat String

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "A dog named " ++ name
describeAnimal (Cat name) = "A cat named " ++ name

dog1 :: Animal
dog1 = Dog "Rex"

cat1 :: Animal
cat1 = Cat "Whiskers"

-- Task 8: Type synonyms and greet function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, my name is " ++ n ++ " and I am " ++ show a ++ " years old."

-- Task 9: Transaction record and creator
data Transaction = Transaction {
  from :: Address,
  to :: Address,
  amount :: Value,
  transactionId :: String
} deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction f t v =
  let tid = "TX-" ++ take 4 f ++ "-" ++ take 4 t ++ "-" ++ show v
  in show (Transaction f t v tid)

-- Task 10: Book with Show
data Book = Book { title :: String, author :: String, year :: Int } deriving Show

book1 :: Book
book1 = Book "Learn You a Haskell" "Miran Lipovača" 2011

-- Main function to display results
main :: IO ()
main = do
  putStrLn "=== HC8 Haskell Chapter 8 Tasks ==="

  -- Task 1
  putStrLn "\nTask 1: generateTx"
  putStrLn (generateTx "addr1" "addr2" 100)

  -- Task 2
  putStrLn "\nTask 2: bob (Person with Cash)"
  print bob

  -- Task 3
  putStrLn "\nTask 3: Area of Circle and Rectangle"
  print (area (Circle 5))
  print (area (Rectangle 10 5))

  -- Task 4
  putStrLn "\nTask 4: Employee richard"
  print richard

  -- Task 5
  putStrLn "\nTask 5: Persons"
  print person1
  print person2

  -- Task 6
  putStrLn "\nTask 6: Circle and Rectangle with record syntax"
  print myCircle
  print myRectangle

  -- Task 7
  putStrLn "\nTask 7: describeAnimal"
  putStrLn (describeAnimal dog1)
  putStrLn (describeAnimal cat1)

  -- Task 8
  putStrLn "\nTask 8: greet"
  putStrLn (greet "Zoe" 25)

  -- Task 9
  putStrLn "\nTask 9: createTransaction"
  putStrLn (createTransaction "sourceAddr" "targetAddr" 500)

  -- Task 10
  putStrLn "\nTask 10: Book"
  print book1
