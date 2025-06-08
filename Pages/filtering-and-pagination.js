const categoryButtons = document.querySelectorAll('[data-filter-type="category"]');
			const languageButtons = document.querySelectorAll('[data-filter-type="language"]');
			const projectCards = document.querySelectorAll('.project-card');
			const paginationContainer = document.getElementById('pagination');

			let selectedCategory = 'all';
			let selectedLanguage = 'all';
			let currentPage = 1;
			const itemsPerPage = 6;

			function updateActive(buttons, value) {
			buttons.forEach(btn => {
				btn.classList.toggle('active', btn.dataset.filterValue === value);
			});
			}

			function getFilteredCards() {
			return Array.from(projectCards).filter(card => {
				const matchesCategory = selectedCategory === 'all' || card.classList.contains(selectedCategory);
				const matchesLanguage = selectedLanguage === 'all' || card.classList.contains(selectedLanguage);
				return matchesCategory && matchesLanguage;
			});
			}

			function renderPagination(filteredCards) {
			paginationContainer.innerHTML = '';
			const totalPages = Math.ceil(filteredCards.length / itemsPerPage);

			if (totalPages <= 1) return; // no need for pagination

			// Previous button
			const prevBtn = document.createElement('button');
			prevBtn.textContent = 'Previous';
			prevBtn.classList.add('pagination-btn');
			prevBtn.disabled = currentPage === 1;
			prevBtn.addEventListener('click', () => {
				if (currentPage > 1) {
				currentPage--;
				filterAndPaginate();
				}
			});
			paginationContainer.appendChild(prevBtn);

			// Page numbers
			for (let i = 1; i <= totalPages; i++) {
				const btn = document.createElement('button');
				btn.textContent = i;
				btn.classList.add('pagination-btn');
				if (i === currentPage) btn.classList.add('active');
				btn.addEventListener('click', () => {
				currentPage = i;
				filterAndPaginate();
				});
				paginationContainer.appendChild(btn);
			}

			// Next button
			const nextBtn = document.createElement('button');
			nextBtn.textContent = 'Next';
			nextBtn.classList.add('pagination-btn');
			nextBtn.disabled = currentPage === totalPages;
			nextBtn.addEventListener('click', () => {
				if (currentPage < totalPages) {
				currentPage++;
				filterAndPaginate();
				}
			});
			paginationContainer.appendChild(nextBtn);
			}

			function filterAndPaginate() {
			const filteredCards = getFilteredCards();

			// Fix edge case: current page is now out of bounds
			const totalPages = Math.ceil(filteredCards.length / itemsPerPage);
			if (currentPage > totalPages) currentPage = totalPages || 1;

			projectCards.forEach(card => card.style.display = 'none');

			const startIndex = (currentPage - 1) * itemsPerPage;
			const endIndex = startIndex + itemsPerPage;

			filteredCards.slice(startIndex, endIndex).forEach(card => {
				card.style.display = 'block';
			});

			renderPagination(filteredCards);
			}

			categoryButtons.forEach(btn => {
			btn.addEventListener('click', () => {
				selectedCategory = btn.dataset.filterValue;
				updateActive(categoryButtons, selectedCategory);
				currentPage = 1;
				filterAndPaginate();
			});
			});

			languageButtons.forEach(btn => {
			btn.addEventListener('click', () => {
				selectedLanguage = btn.dataset.filterValue;
				updateActive(languageButtons, selectedLanguage);
				currentPage = 1;
				filterAndPaginate();
			});
			});

			// Initial render
			filterAndPaginate();