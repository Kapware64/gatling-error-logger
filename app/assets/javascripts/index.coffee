$ ->
  $.get "/errors", (errors) ->
    $.each errors, (index, error) ->
      name = $("<div>").addClass("Name").text error.name
      date = $("<div>").addClass("Date").text error.date
      $("#errors").append $("<li>").append(name).append(date)