/*
  Javascript for the Word Vector Interface
 */

let toggleSideNav = function(e) {
  console.log('Toggling navbar visibility');
  console.log(e);
  let sideMenu = document.getElementById("navbarResponsive");
  if ( sideMenu !== undefined ) {
    copyNavMenu();
  } /*else if ( sideMenu.classList.contains('show') ) {
    
  } else {
    
  }*/
  sideMenu.classList.toggle('show');
};

// Duplicate the collapsed menu in the header.
let copyNavMenu = function() {
  if ( ! document.getElementById("navbarResponsive") ) {
    let menuEl = document.createElement('section'),
        menuList = document.getElementById("navbarResponsiveBase").children[0],
        sidebar = document.getElementById("sidebarCollapsed");
    menuEl.setAttribute('id', 'navbarResponsive');
    menuEl.classList.add('collapse');
    menuEl.append(menuList.cloneNode(true));
    menuEl.addEventListener('shown.bs.collapse', function(e) {
      console.log("SHOW MENU PLZ");
      menuEl.classList.toggle('show');
    });
    sidebar.prepend(menuEl);
    //$('.collapse').collapse();
  }
};

// Create a callback function to be run when the entire document has loaded.
let onLoad = function() {
  console.log("Page loaded");
  copyNavMenu();
  // When the hamburger menu is clicked, toggle the visibility of the nav menu.
  //document.getElementById("sidebar-hider").onclick = toggleSideNav;
};

/* Ensure that the callback function above is run, whether or not the DOM has 
  already been loaded. Solution by Julian Kühnel: 
  https://www.sitepoint.com/jquery-document-ready-plain-javascript/ */
if ( document.readyState === 'complete' 
   || ( document.readyState !== 'loading' && !document.documentElement.doScroll ) 
   ) {
  onLoad();
} else {
  document.addEventListener('DOMContentLoaded', onLoad);
}
/*
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
*/
