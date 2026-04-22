type Radius = Double
type Width = Double
type Height = Double

data Circle = Circle Radius
data Rectangle = Rectangle Width Height | Square Width

data Shape = CircleShape Circle | RectangleShape Rectangle

perimeter :: Shape -> Double
perimeter (RectangleShape (Square width)) = 4 * width
perimeter (RectangleShape (Rectangle width height)) = 2 * (width + height)
perimeter (CircleShape (Circle radius)) = 2 * 3.14 * radius

area :: Shape -> Double
area (RectangleShape (Square width)) = width * width
area (RectangleShape (Rectangle width height)) = width * height
area (CircleShape (Circle radius)) = 3.14 * radius * radius
