console.log("script loaded");


var t = document.getElementById("sidebarCollapsed");
t.classList.add('side-open');


var elem = document.getElementById("sidebar-hidder");
elem.onclick = function() {
        var t = document.getElementById("sidebarCollapsed");

        if (t.classList.contains('side-open')) {
          // The box that we clicked has a class of bad so let's remove it and add the good class
          t.classList.remove('side-open');
          t.classList.add('side-close');
        } else {
          t.classList.add('side-open');
          t.classList.remove('side-close');
          console.log("You can proceed!");
        }

        return false;
    };
