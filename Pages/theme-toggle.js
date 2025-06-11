const savedTheme = localStorage.getItem('theme');

// Apply saved or default theme on load
if (savedTheme === 'theme-light') {
  document.body.classList.add('theme-light');
} else {
  document.body.classList.add('theme-dark');
}

// Add click handlers for each SVG path
document.getElementById('light-mode')?.addEventListener('click', () => {
  document.body.classList.replace('theme-dark', 'theme-light');
  localStorage.setItem('theme', 'theme-light');
});

document.getElementById('dark-mode')?.addEventListener('click', () => {
  document.body.classList.replace('theme-light', 'theme-dark');
  localStorage.setItem('theme', 'theme-dark');
});
