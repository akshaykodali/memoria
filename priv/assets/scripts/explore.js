YUI().use('calendar', 'io-base', 'node', function (Y) {
  var cfg = {
    method: "POST"
  };

  var calendar = new Y.Calendar({
          contentBox: "#archive-calendar",
          minimumDate: new Date(2015,05,17),
          maximumDate: new Date(),
          date: new Date()}).render();

  calendar.on("selectionChange", function(ev) {
    var newDate = ev.newSelection[0];
    var year = newDate.getUTCFullYear(),
      month = newDate.getUTCMonth() + 1,
      day = newDate.getUTCDate();

    cfg.data = "date=" + year + "," + month + "," + day + "";
    makerequest();
  });

  var makerequest = function() {
    var request = Y.io("/explore", cfg);
  };

  Y.on("io:failure", function() { console.log("Unable to process request.") });
  Y.on("io:success", function(ioId, o) {
    var container = Y.one('#experiences');
    container.set("innerHTML", o.responseText);
  });
});