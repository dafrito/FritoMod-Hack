--------------------------------------------------------------------------------
-- Original Author: Eric Tetz <erictetz@gmail.com> 2008
-- Hack. Ingame Lua editing and compiling. Frito. Thonik.

--------------------------------------------------------------------------------

HackDB = { -- default settings saved variables
   font = 2,
   fontsize = 11,
   snap = 1,
   pages = { untitled = {name = "untitled", data='',index=1,} },
   order = {"untitled"}, --list that the index points to the page name
   autoapproved = {},
   sharing = {},
   colorTable = 1
}

Hack = {
   tooltips = {
      HackNew         = 'Create new %s',
      HackDelete      = 'Delete this %s\nSHIFT to skip confirmation prompt',
      HackRename      = 'Rename this %s',
      HackMoveUp      = 'Move this %s up in the list\nSHIFT to move in increments of 5',
      HackMoveDown    = 'Move this %s down in the list\nSHIFT to move in increments of 5',
      HackAutorun     = 'Run this page automatically when Hack loads',
      HackRun         = 'Run this page',
      HackSend        = 'Send this page to another Hack user',
      HackShare       = 'Automatically sync this page with another Hack user\nThis feature is experimental and may not work',
      HackSnap        = 'Attach editor to list window',
      HackEditClose   = 'Close editor for this page',
      HackFontCycle   = 'Cycle through available fonts',
      HackFontBigger  = 'Increase font size',
      HackFontSmaller = 'Decrease font size',
      HackRevert      = 'Revert to saved version of this page',
      HackColorize    = 'Enable Lua syntax highlighting for this page',
      HackSearchEdit  = 'Find %ss matching this text\nENTER to search forward\nSHIFT+ENTER to search backwards',
      HackSearchName  = 'Search %s name',
      HackSearchBody  = 'Search page text',
   },
   fonts = {
      'Interface\\AddOns\\Hack\\Media\\VeraMono.ttf',
      'Interface\\AddOns\\Hack\\Media\\Inconsolata.ttf',
      'Fonts\\FRIZQT__.TTF',
      'Fonts\\ARIALN.TTF',
   },
   colorTables = { -- found in Indent.lua
      HackIndent.hackColorTable,
      HackIndent.defaultColorTable,
   },
   tab = '    ',
   ListItemHeight =  17, -- used in the XML, too
   ListVOffset    =  37, -- vertical space not available for list items
   MinHeight      = 141, -- scroll bar gets wonky if we let the window get too short
   MinWidth       = 296, -- keep buttons from crowding/overlapping
   MaxWidth       = 572, -- tune to match size of 200 character page name
   MaxVisible     =  50, -- num visible without scrolling; limits num HackListItems we must create
   NumVisible     =   0, -- calculated during list resize
}

BINDING_HEADER_HACK = 'Hack'  -- used by binding system

local PLAYERNAME = GetUnitName('player')

function Hack.Upgrade()
local maxVersion = "1.2.4"
if HackDB.version and maxVersion == HackDB.version then return end -- don't need to load tables and shit if not needed
   -- all upgrades need to use functions and variables found only within that upgrade
   -- saved variables will have to be used; that is kind of the point of this
   local upgrades = {
      ["1.1.0"] = function(self) -- from
         if not HackDB.books then HackDB.version = "1.2.0" return end-- maybe they have deleted all their saved vars.
         if not HackDB.order then HackDB.order = {} end -- thought this was taken care of in the default stuff above?
         if not HackDB.pages then HackDB.pages = {} end
         local pages, order = {},{}
         for _,book in ipairs(HackDB.books) do
            for _,page in ipairs(book.data) do
               if not pages[page.name] then -- don't want to overwrite anything
                  pages[page.name] = page -- table[''] is valid!
                  table.insert(order, page.name)
                  pages[page.name].index = #order
               else
                  for i=2,#order+2 do -- first copy is name(2) etc,maybe all things are the same name!
                     if not pages[page.name..'('..i..')'] then
                        local n = page.name..'('..i..')'
                        pages[n] = page
                        pages[n].name = n
                        table.insert(order,n)
                        pages[n].index = #order
                        break
                     end
                  end
               end
            end
         end
         HackDB.books = nil
         HackDB.book = nil
         HackDB.pages = pages
         HackDB.order = order
         HackDB.version = "1.2.0" -- to
      end,
      ["1.2.0"] = function(self)
         if not HackDB.colorTable then HackDB.colorTable = 1 end
      HackDB.version = "1.2.1"
      end,
      ["1.2.1"] = function(self)
         HackDB.autoapproved={}
         HackDB.version = "1.2.2"
      end,
      ["1.2.2"] = function(self)
         HackDB.sharing={}
         HackDB.version = "1.2.3"
      end,
      ["1.2.3"] = function(self)
         for _, senders in pairs(HackDB.sharing) do
            for i=1,#senders do
               senders[senders[i]]=true;
            end;
            while #senders > 0 do
               table.remove(senders);
            end;
         end;
         HackDB.version = "1.2.4"
      end,
   }

   if not HackDB.version then
      HackDB.version = "1.1.0"
   end
   while HackDB.version ~= maxVersion do
      local tempVersion = HackDB.version -- preventing nub infinite loop

      if upgrades[HackDB.version] then
         upgrades[HackDB.version]()
         if tempVersion == HackDB.version then
            error("Continuously trying to upgrade from "..HackDB.version)
         end
      else
         error("Can't upgrade from "..HackDB.version)
      end
   end
end

StaticPopupDialogs.HackAccept = {
   text = 'Accept new Hack page from %s?', button1 = 'Yes', button2 = 'No',
   timeout = 0, whileDead = 1, hideOnEscape = 1,
   OnAccept = function(self)
      Hack.New(self.page, true) -- all received pages start at end of list
      Remote["Hack"][self.sender]("Ack" .. PLAYERNAME);
   end,
   OnCancel = function(self)
      Remote["Hack"][self.sender]("Nack" .. PLAYERNAME);
   end,
}

StaticPopupDialogs.HackSendTo = {
   text = 'Send selected page to', button1 = 'OK', button2 = 'CANCEL',
   hasEditBox = 1, timeout = 0, whileDead = 1, hideOnEscape = 1,
   OnAccept = function(self)
      --XXX local name = getglobal(this:GetParent():GetName()..'EditBox'):GetText()
      local name = self.editBox:GetText()
      if name == '' then return true end
      Hack.SendPage(self.page, 'WHISPER', name)
   end
}

StaticPopupDialogs.HackDelete = {
   text = 'Delete selected %s?', button1 = 'Yes', button2 = 'No',
   timeout = 0, whileDead = 1, hideOnEscape = 1,
   OnAccept = function()
      Hack.DeleteSelected()
   end
}

StaticPopupDialogs.HackAcceptShare = {
   text = "Share '%s' with %s?", button1 = 'Yes', button2 = 'No',
   timeout = 0, whileDead = 1, hideOnEscape = 1,
   OnAccept = function(self)
      Remote["Hack"][self.sender]("AcceptShare" .. PLAYERNAME);
      Hack.AutoApproveUpdates(self.page, self.sender);
   end
}

local db -- alias for HackDB
local pages -- alias for HackDB.pages
local order -- alias for HackDB.order
local selected = nil -- index of selected list item
local autoapproved = nil
local sharing = nil

local function printf(...) DEFAULT_CHAT_FRAME:AddMessage('|cffff6600<Hack>: '..format(...)) end
local function getobj(...) return getglobal(format(...)) end
local function enableButton(b,e) if e then HackNew.Enable(b) else HackNew.Disable(b) end end

-- finds the page for the index
function Hack.Find(index)
   if order[index] then
      return pages[order[index]]
   end
end

-- Thonik: Update to highlight line Num?
function Hack.ScriptError(type, err)
   local name, line, msg = err:match('%[string (".-")%]:(%d+): (.*)')
   printf( '%s error%s:\n %s', type,
          name and format(' in %s at line %d', name, line, msg) or '',
          err )
end

function Hack.Compile(page)
   local func, err = loadstring(page.data:gsub('||','|'), page.name)
   if not func then Hack.ScriptError('syntax', err) return end
   return func
end

-- find page by index or name and return it as a compiled function
function Hack.Get(name)
   local page = type(name)=='number' and Hack.Find(name) or pages[name]
   if not page then printf('attempt to get an invalid page') return end
   return Hack.Compile(page)
end

-- avoids need to create a table to capture return values in Hack.Execute
local function CheckResult(...)
   if ... then return select(2,...) end
   Hack.ScriptError('runtime', select(2,...))
end

function Hack.Execute(func, ...)
   if func then return CheckResult( pcall(func, ...) ) end
end

function Hack.Run(index, ...)
   return Hack.Execute( Hack.Get(index or selected), ... )
end

-- Thonik: Don't fully understand
do
   local loaded = {}
   -- similar to Lua 'require': loads a page if not already loaded
   function Hack.Require(name)
      if not loaded[name] then
         loaded[name] = true
         Hack.Run(name)
      end
   end
end


function Hack.DoAutorun()
   for _,page in pairs(pages) do
      if page.autorun then
         Hack.Execute( Hack.Compile(page) )
      end
   end
end

function Hack.GetUniqueName(name)
   name = name:gsub("%(%d+%)$", "");
   if not pages[name] then
      return name;
   end;
   local count = 2;
   while true do
      local candidate = ("%s(%d)"):format(name, count);
      if not pages[candidate] then
         return candidate;
      end;
      count = count + 1;
   end;
end

function Hack.OnLoad(self)
   -- instantiate list items
   local name = 'HackListItem'
   for i=2,Hack.MaxVisible do
      local li = CreateFrame('Button', name..i, HackListFrame, 'T_HackListItem')
      li:SetPoint('TOP', name..(i-1), 'BOTTOM')
      li:SetID(i)
   end

   Events.VARIABLES_LOADED(Hack.VARIABLES_LOADED, self);
   Remote["Hack"](Hack.CHAT_MSG_ADDON);
   Callbacks.StringChunks(Remote["HackPages"], Hack.INCOMING_PAGE);

   Slash.hack = function(name)
      if name == '' then
         Hack.Toggle()
      else
         Hack.Run(name)
      end
   end

   printf('Loaded. /hack to toggle')
end

function Hack.VARIABLES_LOADED(self)
   Hack.Upgrade()
   db = HackDB
   pages = db.pages
   order = db.order
   autoapproved = db.autoapproved
   sharing = db.sharing
   Hack.UpdateFont()
   Hack.UpdateButtons()
   Hack.UpdateSearchContext()
   HackSnap:SetChecked(HackDB.snap)
   Hack.Snap()
   if not HackIndent then HackColorize:Hide() end
   self:SetMaxResize(Hack.MaxWidth, (Hack.MaxVisible * Hack.ListItemHeight) + Hack.ListVOffset + 5)
   self:SetMinResize(Hack.MinWidth, Hack.MinHeight)
   HackListFrame:SetScript('OnSizeChanged', Hack.UpdateNumListItemsVisible)
   Hack.UpdateNumListItemsVisible()
   Hack.DoAutorun()
end

function Hack.SelectListItem(index)
   selected = index
   Hack.UpdateButtons()
   Hack.EditPage()
end

local function ListItemClickCommon(id, op)
   PlaySound('igMainMenuOptionCheckBoxOn')
   op(id + FauxScrollFrame_GetOffset(HackListScrollFrame))
   Hack.UpdateListItems()
end

function Hack.OnListItemClicked(id)
   ListItemClickCommon(id, Hack.SelectListItem)
end

function Hack.OnListItemAutorunClicked(id, enable)
   ListItemClickCommon(id, function(selected) pages[order[selected]].autorun = enable end)
end

function Hack.UpdateNumListItemsVisible()
   local visible = math.floor( (HackListFrame:GetHeight()-Hack.ListVOffset) / Hack.ListItemHeight )
   Hack.NumVisible = math.min( Hack.MaxVisible, visible )
   Hack.UpdateListItems()
end

function Hack.UpdateListItems()
   local scrollFrameWidth = HackListFrame:GetWidth() - 18 -- N = inset from right edge

   FauxScrollFrame_Update(HackListScrollFrame, #order, Hack.NumVisible, Hack.ListItemHeight,
      nil, nil, nil, HackListScrollFrame, scrollFrameWidth-17, scrollFrameWidth) -- N = room for scrollbar
   local offset = FauxScrollFrame_GetOffset(HackListScrollFrame)
   for widgetIndex=1, Hack.MaxVisible do
      local itemIndex = offset + widgetIndex
      local item = pages[order[itemIndex]]
      local widget = getobj('HackListItem%d', widgetIndex)
      if not item or widgetIndex > Hack.NumVisible then
         widget:Hide()
      else
         widget:Show()
         local name = getobj('HackListItem%dName', widgetIndex)
         local edit = getobj('HackListItem%dEdit', widgetIndex)
         local auto = getobj('HackListItem%dAutorun', widgetIndex)
         edit:ClearFocus() -- in case someone tries to scroll while renaming
           if Hack.SearchMatch(item) then
            name:SetTextColor(1,1,1) else name:SetTextColor(.3,.3,.3) end
         if itemIndex == selected then
            widget:LockHighlight() else widget:UnlockHighlight() end
         auto:Show()
         name:SetText(item.name)
         auto:SetChecked(item.autorun)
      end
   end
end

-- Basically got the following from WoWLua
-- Adding Line Numbers to the EditPage
function Hack:UpdateLineNums()
        --could edit it to pass a variable and highlight a line


   -- Since this can be FAIAP enabled, we need to pass true in order
   -- to get the raw values
   local editbox = HackEditBox
   local linebox = HackLineNumEditBoxFrame
   local linescroll = HackLineNumScrollFrame
   local linetest = HackEditBox:CreateFontString()
    linetest:SetFont(Hack.fonts[db.font], db.fontsize)


   local width = editbox:GetWidth() -65 --accounting for text insets in the xml
   local text = editbox:GetText(true)

   local linetext = ""
   local count = 1
   for line in text:gmatch("([^\n]*\n?)") do
            if #line > 0 then
            --will highlight if I ever put it in
          --linetext = linetext .. "|cFFFF1111" .. count .. "|r" .. "\n"
         linetext = linetext .. count .. "\n"
         count = count + 1

         -- Check to see if the line of text spans more than one actual line
         linetest:SetText(line:gsub("|", "||"))
         local testwidth = linetest:GetWidth()
         if testwidth >= width then
            linetext = linetext .. string.rep("\n", math.floor(testwidth / width))
         end
      end
   end
--[[
        --what is this doing?
   if text:sub(-1, -1) == "\n" then
      linetext = linetext .. count .. "\n"
      count = count + 1
   end
       --]]


   -- Make the line number frame wider as necessary
        linetest:SetText(count)
   local numwidth = linetest:GetWidth()
    --always a 3 pixel buffer between the number and the main editbox
   linescroll:SetWidth(3+numwidth)
   linebox:SetWidth(3+numwidth)

    --apply what we've done
   linebox:SetText(linetext)

end




function Hack.UpdateButtons()
   enableButton( HackDelete,   selected )
   enableButton( HackRename,   selected )
   enableButton( HackSend,     selected )
   enableButton( HackMoveUp,   selected and selected > 1 )
   enableButton( HackMoveDown, selected and selected < #order )
end

function Hack.UpdateSearchContext()
   local pattern = HackSearchEdit:GetText()
      :gsub('[%[%]%%()]', '%%%1') -- escape magic chars (the price we pay for real-time filtering)
      :gsub('%a', function(c) return format('[%s%s]', c:lower(), c:upper()) end) -- case insensitive
   local nx, bx = HackSearchName:GetChecked(), HackSearchBody:GetChecked()
   function Hack.SearchMatch(item)
      return not (nx or bx)
             or (nx and item.name:match(pattern))-- searching names
             or (bx and item.data:match(pattern)) -- searching inside pages
   end
   Hack.UpdateListItems()
end

function Hack.DoSearch(direction) -- 1=down, -1=up
   if #order == 0 then return end
   local start = selected or 1
   local it = start
   repeat
      it = it + direction
      if     it > #order then it = 1 -- wrap at..
      elseif it < 1 then it = #order --   ..either end
      end
      if Hack.SearchMatch(order[it]) then
         Hack.SelectListItem(it)
         Hack.ScrollSelectedIntoView()
         HackSearchEdit:SetFocus()
         break
      end
   until it == start
end

function Hack.ScrollSelectedIntoView()
   local offset = FauxScrollFrame_GetOffset(HackListScrollFrame)
   local id = selected - offset
   if     id >  Hack.NumVisible then offset = selected-Hack.NumVisible
   elseif id <= 0                then offset = selected-1 end
   FauxScrollFrame_SetOffset(HackListScrollFrame, offset)
   HackListScrollFrameScrollBar:SetValue(offset * Hack.ListItemHeight)
   Hack.UpdateListItems()
end

function Hack.Toggle(msg)
   if HackListFrame:IsVisible() then
      HackListFrame:Hide()
   else
      HackListFrame:Show()
   end
end

function Hack.Tooltip(self)
   local which = self:GetName()
   local tip
   if which and which:match('Autorun') then
      tip = 'Automatically run this page when Hack loads'
   elseif Hack.tooltips[which] then
      tip = format(Hack.tooltips[which], "page")
   else
      return;
   end;
   GameTooltip:SetOwner(self, 'ANCHOR_RIGHT')
   GameTooltip:AddLine(tip)
   GameTooltip:Show()
end


function Hack.Rename()
   local id = selected - FauxScrollFrame_GetOffset(HackListScrollFrame)
   local name = getobj("HackListItem%dName", id)
   local edit = getobj("HackListItem%dEdit", id)
   edit:SetText( pages[order[selected]].name )
   edit:Show()
   edit:SetCursorPosition(0)
   edit:SetFocus()
   name:Hide()
end

function Hack.FinishRename(name, editbox)
   name = Hack.GetUniqueName(name)
   pages[name] = pages[order[selected]]
   pages[name].name = name
   pages[order[selected]] = nil  --its happening
   order[selected] = name
   Hack.UpdateListItems()
end

function Hack.New(page, atEnd)
   local index = (atEnd and #order+1) or  selected and selected+1 or #order+1
   if page then
      page.name = Hack.GetUniqueName(page.name)
   else
      page = {name = Hack.GetUniqueName(''), data='' }
   end

   pages[page.name] = page
   table.insert(order, index, page.name)
   pages[page.name].index = #order

   Hack.SelectListItem(index)
   Hack.UpdateListItems()
   Hack.ScrollSelectedIntoView()
   if HackListFrame:IsShown() then Hack.Rename() end
end

function Hack.Delete()
   if IsShiftKeyDown() or #pages[order[selected]].data == 0 then
      Hack.DeleteSelected()
   else
      StaticPopup_Show('HackDelete', "page")
   end
end

function Hack.DeleteSelected()
   HackEditFrame:Hide()
   pages[order[selected]] = nil
   table.remove(order,selected)
   if #order == 0 then selected = nil
   elseif selected > #order then selected = #order end
   Hack.UpdateButtons()
   Hack.UpdateListItems()
end

function Hack.Revert()
   HackEditBox:SetText(Hack.revert)
   HackEditBox:SetCursorPosition(0)
   HackRevert:Disable()
end

function Hack.MoveItem(direction)
   local to = selected + direction * (IsShiftKeyDown() and 5 or 1)
   if     to > #order then to = #order
   elseif to < 1      then to = 1      end
   while selected ~= to do
      order[selected], order[selected+direction] = order[selected+direction], order[selected]
      selected = selected + direction
   end
   for i=1,#order do
      pages[order[i]].index = i --updating the index property of the pages
   end
   Hack.ScrollSelectedIntoView()
   Hack.UpdateButtons()
end

function Hack.MoveUp()
   Hack.MoveItem(-1)
end

function Hack.MoveDown()
   Hack.MoveItem(1)
end

function Hack.FontBigger()
   db.fontsize = db.fontsize + 1
   Hack.UpdateFont()
end

function Hack.FontSmaller()
   db.fontsize = db.fontsize - 1
   Hack.UpdateFont()
end

function Hack.FontCycle()
   db.font = (db.font < #Hack.fonts) and (db.font + 1) or (1)
   Hack.UpdateFont()
end
-- currently unattached to any in-game config
function Hack.ColorTableCycle()
   db.colorTable = (db.colorTable < #Hack.colorTables) and (db.colorTable+1) or (1)
   Hack.ApplyColor(true)
end

function Hack.UpdateFont()
   HackEditBox:SetFont(Hack.fonts[db.font], db.fontsize)
   HackLineNumEditBoxFrame:SetFont(Hack.fonts[db.font], db.fontsize)
end

function Hack.OnButtonClick(name)
   trace("Hack button clicked: " .. (name or ""));
   Hack[ name:match('Hack(.*)') ]()
end

function Hack.ApplyColor(colorize)
   if colorize then
      HackIndent.enable(HackEditBox,Hack.colorTables[db.colorTable], 3)
      HackIndent.colorCodeEditbox(HackEditBox)
   else
      HackIndent.disable(HackEditBox)
   end
end

function Hack.EditedPage()
    return Hack.editedPage;
end;

function Hack.EditPage()
   local page = pages[order[selected]]
   Hack.editedPage = page;
   Hack.revert = page.data
   HackEditBox:SetText(page.data)
   HackRevert:Disable()
   HackEditFrame:Show()
   HackEditBox:SetCursorPosition(0)
   if HackIndent then
      HackColorize:SetChecked(page.colorize)
      Hack.ApplyColor(page.colorize)
   end
end

function Hack.SendPageToWatchers(page)
   if not page then
      page=Hack.EditedPage();
   end;
   if not page then
      return;
   end;
   for watcher,_ in pairs(sharing[page.name]) do
      Hack.SendPage(page, "WHISPER", watcher);
   end;
end;

local shareMyPage=Timing.Cooldown(.25, Hack.SendPageToWatchers);
function Hack.OnEditorTextChanged(self, isUserInput)
   local page = pages[order[selected]]
   page.data = self:GetText()
   enableButton(HackRevert, page.data ~= Hack.revert)
   if not HackEditScrollFrameScrollBarThumbTexture:IsVisible() then
      HackEditScrollFrameScrollBar:Hide()
   end
   Hack.UpdateLineNums();
   if isUserInput and sharing[page.name] then
      shareMyPage();
   end;
end

function Hack.OnEditorShow()
   Hack.MakeESCable('HackListFrame',false)
   PlaySound('igQuestListOpen')
end

function Hack.OnEditorHide()
   Hack.MakeESCable('HackListFrame',true)
   PlaySound('igQuestListClose')
end

function Hack.OnEditorLoad(self)
   table.insert(UISpecialFrames,'HackEditFrame')
   self:SetMinResize(Hack.MinWidth,Hack.MinHeight)
   HackEditBox:SetScript("OnTextChanged", function(self, isUserInput)
      ScrollingEdit_OnTextChanged(self, self:GetParent())
      Hack.OnEditorTextChanged(self, isUserInput)
   end);
end

function Hack.Snap()
   HackDB.snap = HackSnap:GetChecked()
   if HackDB.snap then
      HackEditFrame:ClearAllPoints()
      HackEditFrame:SetPoint('TOPLEFT', HackListFrame, 'TOPRIGHT', -2, 0)
   end
end

function Hack.Colorize()
   local page = pages[order[selected]]
   page.colorize = HackColorize:GetChecked()
   Hack.ApplyColor(page.colorize)
end

function Hack.SelectedPage()
    return pages[order[selected]];
end;

do
   local function send(self) Hack.SendPage(Hack.SelectedPage(), self.value) end
   local menu = {
      { text = 'Player', func = function()
            local dialog = StaticPopup_Show('HackSendTo')
            if dialog then
               dialog.page = pages[order[selected]]
               dialog.editBox:SetScript('OnEnterPressed',  function(t) dialog.button1:Click() end)
            end
         end
      },
      { text = 'Party', func = send },
      { text = 'Raid',  func = send },
      { text = 'Guild', func = send },
   }
   CreateFrame('Frame', 'HackSendMenu', HackListFrame, 'UIDropDownMenuTemplate')
   function Hack.Send()
      menu[2].disabled = GetNumPartyMembers() == 0
      menu[3].disabled = not UnitInRaid('player')
      menu[4].disabled = not IsInGuild()
      EasyMenu(menu, HackSendMenu, 'cursor', nil, nil, 'MENU')
   end
end

local i=0;
function Hack.SendPage(page, channel, name)
   trace("Sending '%s' to %s", page.name, name or channel);
   Remote["HackPages"][name or channel](
      Serializers.WriteStringChunks(
         Serializers.WriteData(page), "HackPages"));
end

function Hack.CHAT_MSG_ADDON(msg, sender, medium)
   if sender == PLAYERNAME then return end

   local responders = {};
   function responders.Ack()
      printf('%s accepted your page.', sender)
   end;
   function responders.Nack()
      printf('%s rejected your page.', sender)
   end;
   function responders.Share()
      printf('Received %s from %s', body, sender);
      local dialog=StaticPopup_Show('HackAcceptShare', body, sender);
      dialog.page=body;
      dialog.sender=sender;
   end;
   function responders.AcceptShare()
      -- TODO People could "steal" pages since we don't record what _we_ want to send.
      -- TODO We don't have a way to stop sharing.
      assert(pages[body], "Page could not be found with name: "..body);
      if not sharing[body] then
         sharing[body]={};
      end;
      sharing[body][sender]=true;
   end;

   for cmd, handler in pairs(responders) do
      if Strings.StartsWith(msg, cmd) then
         handler(msg:match("^"..cmd.."(.*)$"));
         return;
      end;
   end;
   print("Message Not handled: " .. msg);
end;

function Hack.INCOMING_PAGE(msg, sender, medium)
   trace("Received page %q", msg);
   local page = Serializers.ReadData(msg);
   assert(page, "Received page must not be falsy (type was "..type(page)..")");
   assert(type(page) == "table", "Received page must be a table, but received ".. type(page));
   if autoapproved[page.name] then
      assert(pages[page.name], "Page could not be found with name: "..page.name);
      pages[page.name].data=page.data;
      if Hack.EditedPage() and Hack.EditedPage().name==page.name then
         HackEditBox:SetText(page.data)
      end;
   else
      page.name=Hack.GetUniqueName(page.name);
      local dialog = StaticPopup_Show('HackAccept', sender)
      if dialog then
         dialog.page = page
         dialog.sender = sender
      end
   end;
end;

function Hack.Share(channel, target)
   if not channel then
      assert(UnitName("target"), "You must have a target to share scripts");
      Hack.Share("WHISPER", UnitName("target"));
      return;
   end;
   Remote["Hack"][target or channel]("HackShare"..Hack.EditedPage().name);
end;

function Hack.AutoApproveUpdates(page, sender)
   -- XXX We ignore the sender here completely.
   autoapproved[page]=true;
end;

-- add/remove frame from UISpecialFrames (borrowed from TinyPad)
function Hack.MakeESCable(frame,enable)
   local index
   for i=1,#UISpecialFrames do
      if UISpecialFrames[i]==frame then
         index = i
         break
      end
   end
   if index and not enable then
      table.remove(UISpecialFrames,index)
   elseif not index and enable then
      table.insert(UISpecialFrames,1,frame)
   end
end

-- vim: set ts=3 sw=3 et :
