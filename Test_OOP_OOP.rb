require_relative 'ruby_OOP.rb'

class Simpletest
  @@test_n = 0

  def initialize
    @bmw = Cars.new("i4", 2015)
    @@test_n += 1
  end

  def self.howmanytests
    @@test_n
  end
end

test1 = Simpletest.new
test2 = Simpletest.new
test3 = Simpletest.new
puts Simpletest.howmanytests # Isso imprimirá 3
