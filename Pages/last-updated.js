const lastModified = new Date(document.lastModified);
const options = { year: 'numeric', month: 'long', day: 'numeric' };
const formattedDate = lastModified.toLocaleDateString('en-US', options);
document.addEventListener("DOMContentLoaded", () => {
    const el = document.getElementById("last_updated");
    if (el) el.textContent = "Last updated: " + formattedDate;
});