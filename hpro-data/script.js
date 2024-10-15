// You'll need to implement the JavaScript for:
// - Theme toggle functionality
// - Section collapse functionality
// - Showing/hiding pages based on navigation clicks

const themeSwitch = document.getElementById('theme-switch');

themeSwitch.addEventListener('change', () => {
    document.body.classList.toggle('dark-theme');
});

function updateOnThisPageLinks() {
    const activePage = document.querySelector('.page.active');
    const headings = activePage.querySelectorAll('h2, h3'); // Adjust if needed

    const linksList = document.getElementById('on-this-page-links');
    linksList.innerHTML = ''; // Clear previous links

    headings.forEach(heading => {
        const link = document.createElement('a');
        link.href = `#${heading.id}`; // Assuming headings have IDs
        link.textContent = heading.textContent;
        
        const listItem = document.createElement('li');
        listItem.appendChild(link);
        linksList.appendChild(listItem);
    });
}

// Call updateOnThisPageLinks() whenever the page content changes


