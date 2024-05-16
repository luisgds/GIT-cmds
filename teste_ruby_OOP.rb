require_relative 'ruby_OOP.rb'
cars = [
bmw = Cars.new("i4", 2010),
mercedes = Cars.new("bens", 1985),
audi = Cars.new("GTR", 2099)
]
for x in cars
  if x.new?
    puts "Your car is new"
  else
    puts "Your car is old"
  end
end
