$ ->
  $.get "/persons", (persons) ->
    $.each persons, (index, person) ->
      name = $("<div>").addClass("Name").text person.name
      age = $("<div>").addClass("Age").text person.age
      gender = $("<div>").addClass("Gender").text person.gender
      $("#persons").append $("<li>").append(name).append(age)