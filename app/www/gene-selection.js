/* Gene identity is always gene_id. Query responses only add candidates; they
   never select a value. Epoch, revision and intent sequence reject stale work. */
(function () {
  let state = {epoch: -1, revision: -1, intent: 0, query: 0, xhr: null};
  window.rsGeneIntent = function (event) {
    event.client_seq = ++state.intent;
    event.epoch = state.epoch;
    return event;
  };
  function install() {
    Shiny.addCustomMessageHandler('rs_gene_selection', function (msg) {
      const el = document.getElementById('gene_pick');
      const s = el && el.selectize;
      if (!s || msg.epoch < state.epoch ||
          (msg.epoch === state.epoch && msg.revision < state.revision)) return;
      if (msg.epoch === state.epoch && msg.client_seq < state.intent) return;
      const changed = msg.epoch !== state.epoch;
      if (changed) {
        if (state.xhr) state.xhr.abort();
        state = {epoch: msg.epoch, revision: -1, intent: msg.client_seq, query: 0, xhr: null};
        s.clear(true); s.clearOptions(true); s.loadedSearches = {};
      }
      state.revision = msg.revision;
      state.analysis = msg.analysis_id; state.arm = msg.intervention;
      state.url = msg.url || state.url;
      s.settings.maxOptions = 100;
      s.settings.load = function (query, callback) {
        const epoch = state.epoch, ticket = ++state.query;
        if (state.xhr) state.xhr.abort();
        state.xhr = $.ajax({url: state.url, data: {query: query}, dataType: 'json',
          success: function (rows) {
            if (epoch !== state.epoch || ticket !== state.query) {callback(); return;}
            Object.keys(s.options).forEach(function (key) {
              if (!s.items.includes(key)) s.removeOption(key, true);
            });
            callback(rows);
          }, error: function () {callback();}});
      };
      if (!s.rsIdentityHandler) {
        s.rsIdentityHandler = true;
        s.on('change', function (gene) {
          if (!gene) return;
          Shiny.setInputValue('gene_pick_event', window.rsGeneIntent({gene_id: gene,
            analysis_id: state.analysis, intervention: state.arm}), {priority:'event'});
        });
      }
      if (msg.gene_id) {
        s.addOption({value: msg.gene_id, label: msg.label});
        s.updateOption(msg.gene_id, {value: msg.gene_id, label: msg.label});
        s.setValue(msg.gene_id, true);
      }
      // A bounded initial search enables mouse/keyboard browsing immediately.
      if (changed) s.load(function (callback) {s.settings.load('', callback);});
      el.dataset.geneEpoch = String(state.epoch);
      el.dataset.geneRevision = String(state.revision);
    });
  }
  if (window.Shiny) install(); else $(document).one('shiny:connected', install);
})();
