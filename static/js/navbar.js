
function displayScrollToTop() {
    var root = document.documentElement;
    // don't bother if the screen is small
    if (root.scrollHeight / root.clientHeight < 5) {
        return;
    }
    var maxScroll = root.scrollHeight - root.clientHeight
    var screensScrolled = root.scrollTop / root.clientHeight;
    var button = document.getElementById("scroll-to-top");
    button.style.display = screensScrolled > 1 ? 'block' : 'none';
}

function scrollToTop() {
    root = document.documentElement.scrollTo(0, 0);
}

function registerNavigationElements() {
    // hamburger expando for navbar on a generic page
    const nav = document.querySelector('#main-navbar');
    const burger = nav.querySelector('.navbar-burger');
    const menu = nav.querySelector('.navbar-menu')
    burger.addEventListener('click', () => {
        burger.classList.toggle('is-active');
        menu.classList.toggle('is-active');
    });

    // scroll-to-top effect
    document.getElementById("scroll-to-top").addEventListener("click", scrollToTop);
}
 
document.addEventListener("scroll", displayScrollToTop);

document.addEventListener('DOMContentLoaded', registerNavigationElements);
