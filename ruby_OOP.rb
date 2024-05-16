class Cars
  def initialize(attribute, year_car)
    @attribute = { name_car: attribute, year: year_car }
  end

  attr_accessor :attribute_name

  def new?
    if @attribute[:year] <= 1990
      false
    else
      true
    end
  end
end
