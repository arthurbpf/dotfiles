" No indent bc of EOFs
if has('nvim')
set completeopt=menu,menuone,noselect
let g:vsnip_filetypes = {}

lua <<EOF
  -- Setup nvim-cmp.
  local cmp = require'cmp'

  cmp.setup({
	snippet = {
	  -- REQUIRED - you must specify a snippet engine
	  expand = function(args)
		vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
		-- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
		-- require('snippy').expand_snippet(args.body) -- For `snippy` users.
		-- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
	  end,
	},
	window = {
	  completion = cmp.config.window.bordered(),
	  documentation = cmp.config.window.bordered(),
	},
	mapping = cmp.mapping.preset.insert({
	  ['<C-b>'] = cmp.mapping.scroll_docs(-4),
	  ['<C-f>'] = cmp.mapping.scroll_docs(4),
	  ['<C-Space>'] = cmp.mapping.complete(),
	  ['<C-e>'] = cmp.mapping.abort(),
	  ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
	}),
	sources = cmp.config.sources({
	  -- { name = 'cmp_tabnine' },
	  { name = 'orgmode' },
	  { name = 'nvim_lsp' },
	  { name = 'vsnip' },
	  { name = 'buffer' },
	})
  })

  -- nvim-lspconfig from here on
  local opts = { noremap=true, silent=true }
  local function config(_config)
	return vim.tbl_deep_extend("force", {
		capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities()),
		on_attach = function(client, bufnr)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gD', ':lua vim.lsp.buf.declaration()<CR>', opts)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>gd', ':lua vim.lsp.buf.definition()<CR>', opts)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', ':lua vim.lsp.buf.hover()<CR>', opts)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>vws', ':lua vim.lsp.buf.workspace_symbol()<CR>', opts)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>vd', ':lua vim.diagnostic.open_float()<CR>', opts)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', '[d', ':lua vim.lsp.diagnostic.goto_next()<CR>', opts)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', ']d', ':lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ca', ':lua vim.lsp.buf.code_action()<CR>', opts)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rr', ':lua vim.lsp.buf.references()<CR>', opts)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rn', ':lua vim.lsp.buf.rename()<CR>', opts)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-h>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
		end,
	}, _config or {})
  end

  require('lspconfig')['pyright'].setup(config())
  require('lspconfig')['tsserver'].setup(config())
  require('lspconfig')['jsonls'].setup(config())
  require('lspconfig')['clangd'].setup(config())
  require('lspconfig')['texlab'].setup(config())
  require('lspconfig')['emmet_ls'].setup(config({
	filetypes = { 'html', 'typescriptreact', 'javascriptreact', 'css', 'sass', 'scss', 'less' }
  }))
  require('lspconfig')['cssls'].setup(config())
EOF
endif
