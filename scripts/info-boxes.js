window.populateInfoBoxes = function(containerId, labels, content) {
  const info_boxes = document.getElementById(containerId);

  if (!info_boxes) return;

  let buttons = "";

  for (let i = 0; i < labels.length; i++) {
    let button_style = "";

    if (i === labels.length - 1) {
      button_style += "border-right: 2px solid #00205B; border-top-right-radius: 0.5rem; border-bottom-right-radius: 0.5rem;";
    }

    if (i === 0) {
      button_style += "border-top-left-radius: 0.5rem; border-bottom-left-radius: 0.5rem;";
    }

    buttons += `
      <div class="flex-fill p-0">
        <h2 class="accordion-header h-100" role="heading">
          <button
            class="accordion-button collapsed h-100 info-tab-btn"
            type="button"
            style="${button_style}"
            data-index="${i}"
            aria-expanded="false"
            aria-controls="${containerId}-collapse"
          >
            ${labels[i]}
          </button>
        </h2>
      </div>
    `;
  }

  info_boxes.innerHTML = `
    <div class="row justify-content-center">
      <div class="col-12 col-xl-8 accordion py-4" id="${containerId}-accordion">

        <div class="d-flex flex-row w-100 info-tab-row">
          ${buttons}
        </div>

        <div class="info-card-wrap">
          <div id="${containerId}-card" class="card my-3">

            <div
              id="${containerId}-collapse"
              class="accordion-collapse collapse"
              data-active-index=""
            >
              <div class="accordion-body">
                <h2 id="${containerId}-title" style="color:#00205B;"></h2>
                <div id="${containerId}-body"></div>
              </div>
            </div>

          </div>
        </div>

      </div>
    </div>
  `;

  const collapseEl = document.getElementById(`${containerId}-collapse`);
  const titleEl = document.getElementById(`${containerId}-title`);
  const bodyEl = document.getElementById(`${containerId}-body`);
  const btns = info_boxes.querySelectorAll(".info-tab-btn");

  const bsCollapse = bootstrap.Collapse.getOrCreateInstance(collapseEl, {
    toggle: false
  });

  function setActiveButton(activeIdx) {
    btns.forEach((button, idx) => {
      const isActive = idx === activeIdx;

      button.classList.toggle("collapsed", !isActive);
      button.setAttribute("aria-expanded", String(isActive));
    });
  }

  function setContent(idx) {
    titleEl.textContent = labels[idx];
    bodyEl.innerHTML = content[idx];
    collapseEl.dataset.activeIndex = String(idx);
  }

  btns.forEach((btn) => {
    btn.addEventListener("click", () => {
      const idx = Number(btn.dataset.index);
      const isOpen = collapseEl.classList.contains("show");
      const activeIdx =
        collapseEl.dataset.activeIndex === ""
          ? null
          : Number(collapseEl.dataset.activeIndex);

      if (!isOpen) {
        setContent(idx);
        setActiveButton(idx);
        bsCollapse.show();
        return;
      }

      if (activeIdx === idx) {
        setActiveButton(-1);
        bsCollapse.hide();
        collapseEl.dataset.activeIndex = "";
        return;
      }

      setContent(idx);
      setActiveButton(idx);
    });
  });

  collapseEl.addEventListener("hidden.bs.collapse", () => {
    btns.forEach((button) => {
      button.classList.add("collapsed");
      button.setAttribute("aria-expanded", "false");
    });
  });
};