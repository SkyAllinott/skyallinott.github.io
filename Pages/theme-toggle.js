const savedTheme = localStorage.getItem('theme') || 'theme-dark';

const iconLight = document.getElementById('icon-light');
const iconDark = document.getElementById('icon-dark');

function applyTheme(theme) {
  document.documentElement.classList.remove('theme-light', 'theme-dark');
  document.documentElement.classList.add(theme);
  localStorage.setItem('theme', theme);

  if (theme === 'theme-light') {
    if (iconLight) iconLight.style.display = 'inline';
    if (iconDark) iconDark.style.display = 'none';
  } else {
    if (iconLight) iconLight.style.display = 'none';
    if (iconDark) iconDark.style.display = 'inline';
  }
}

// Apply saved or default theme on load
applyTheme(savedTheme);

// Add click handlers for each SVG path
document.querySelectorAll('#light-mode').forEach(el =>
  el.addEventListener('click', () => applyTheme('theme-light'))
);

document.querySelectorAll('#dark-mode').forEach(el =>
  el.addEventListener('click', () => applyTheme('theme-dark'))
);