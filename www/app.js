// Front-end interactions: busy bar, image zoom, dropzone highlight, step-nav scrollspy
(function () {
  // Loading indicator tied to Shiny's real busy state
  $(document).on('shiny:busy', function () { document.body.classList.add('shiny-busy-on'); });
  $(document).on('shiny:idle', function () { document.body.classList.remove('shiny-busy-on'); });

  // Click / Enter on any .zoomable image -> enlarged modal (Esc closes)
  function openZoom(src) {
    var m = document.getElementById('zoom-modal');
    document.getElementById('zoom-img').src = src;
    m.classList.add('open');
  }
  function closeZoom() { document.getElementById('zoom-modal').classList.remove('open'); }
  $(document).on('click', 'img.zoomable', function (e) { e.stopPropagation(); openZoom(this.src); });
  $(document).on('keydown', 'img.zoomable', function (e) { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); openZoom(this.src); } });
  $(document).on('click', '#zoom-modal', closeZoom);
  $(document).on('keydown', function (e) { if (e.key === 'Escape') closeZoom(); });

  // Drag & drop highlight (Shiny's fileInput already accepts dropped files)
  $(document).on('dragenter dragover', '.dropzone', function () { $(this).addClass('dragover'); });
  $(document).on('dragleave drop', '.dropzone', function () { $(this).removeClass('dragover'); });

  // Step navigation: smooth scroll + highlight current section
  $(document).on('click', '.step-link', function (e) {
    var t = document.querySelector(this.getAttribute('href'));
    if (t) { e.preventDefault(); t.scrollIntoView({ behavior: 'smooth', block: 'start' }); }
  });
  window.addEventListener('scroll', function () {
    var links = document.querySelectorAll('.step-link');
    if (!links.length) return;
    var current = null;
    document.querySelectorAll('.step-section').forEach(function (s) {
      if (s.offsetParent !== null && s.getBoundingClientRect().top < 180) current = s.id;
    });
    links.forEach(function (a) { a.classList.toggle('active', a.getAttribute('href') === '#' + current); });
  }, { passive: true });

  // Plotly charts inside hidden tabs need a resize when the tab becomes visible
  $(document).on('shown.bs.tab', function () { window.dispatchEvent(new Event('resize')); });
})();
