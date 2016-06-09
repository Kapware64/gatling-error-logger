$ ->
  $.get "/persons", (persons) ->
    $.each persons, (index, person) ->
      name = $("<div>").addClass("name").text person.name
      age = $("<div>").addClass("age").text person.age
      desc = $("<div>").addClass("desc").text person.desc
      $("#persons").append $("<li>").append(name).append(age)