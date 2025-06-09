const toggleBtn = document.getElementById('theme-toggle');
const savedTheme = localStorage.getItem('theme');

if (savedTheme === 'theme-light') {
    document.body.classList.add('theme-light');
} else {
    document.body.classList.add('theme-dark');
}

toggleBtn.addEventListener('click', () => {
    const body = document.body;

    if (body.classList.contains('theme-dark')) {
    body.classList.replace('theme-dark', 'theme-light');
    localStorage.setItem('theme', 'theme-light');
    } else {
    body.classList.replace('theme-light', 'theme-dark');
    localStorage.setItem('theme', 'theme-dark');
    }
});