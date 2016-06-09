$ ->
  $.get "/persons", (persons) ->
    $.each persons, (index, person) ->
      name = $("<div>").addClass("Name").text person.name
      age = $("<div>").addClass("Age").text person.age
      desc = $("<div>").addClass("Details").text person.desc
      $("#persons").append $("<li>").append(name).append(age)