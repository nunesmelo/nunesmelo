program temp_conversition
    implicit none
    real ::fahrenheit,celsius
      print*,"type value celsius"
    read*,celsius
      print*,"celsius"
    fahrenheit=1.8*celsius+32.0
      print*, "fahrenheit=", fahrenheit
end program temp_conversition