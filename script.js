/*
  Javascript for the Word Vector Interface
 */

/* Toggle the Bootstrap v4 "show" class on the sidebar menu. Currently unused. 
  Instead, the v3 "in" class styling is replicated in wvi.css. */
let toggleSideNav = function(e) {
  console.log('Toggling navbar visibility');
  let sideMenu = document.getElementById("navbarResponsive");
  if ( sideMenu !== undefined ) {
    copyNavMenu();
  }
  sideMenu.classList.toggle('show');
};

/* Duplicate the collapsed menu in the header. */
let copyNavMenu = function() {
  if ( ! document.getElementById("navbarResponsive") ) {
    let menuEl = document.createElement('section'),
        menuList = document.getElementById("navbarResponsiveBase").children[0],
        sidebar = document.getElementById("sidebarCollapsed");
    menuEl.setAttribute('id', 'navbarResponsive');
    menuEl.classList.add('collapse');
    menuEl.append(menuList.cloneNode(true));
    sidebar.prepend(menuEl);
  }
};

/* Create a callback function to be run when the entire document has loaded. */
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
