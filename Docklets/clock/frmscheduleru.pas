unit frmscheduleru;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Forms, Classes, Controls, ComCtrls, StdCtrls, EditBtn,
  DateUtils, Math;

type

  _task = packed record
    active: boolean;
    next_time: TDateTime;
    //
    timer: boolean;
    once: boolean;
    daily: boolean;
    timer_time: TDateTime;
    once_date_time: TDateTime;
    repeat_start: TDateTime;
    repeat_end: TDateTime;
    repeat_interval: integer;
    use_repeat_start_date: boolean;
    use_repeat_end_time: boolean;
    use_repeat_end_date: boolean;
    repeat_dow: integer;
    //
    cmd: string;
    params: string;
    dir: string;
    confirm: boolean;
    minimize: boolean;
    message: boolean;
    sound: boolean;
    shutdown: boolean;
    force: boolean;
    message_text: string;
    sound_file: string;
  end;

  { Tfrmscheduler }

  Tfrmscheduler = class(TForm)
    GroupBox1: TGroupBox;
    btn_start: TButton;
    btn_cancel: TButton;
    dtp_once_date: TDateEdit;
    dtp_timer: TDateEdit;
    rb_timer: TRadioButton;
    dtp_once_time: TDateEdit;
    rb_once: TRadioButton;
    rb_daily: TRadioButton;
    GroupBox2: TGroupBox;
    dtp_start_time: TDateEdit;
    dtp_end_time: TDateEdit;
    dtp_start_date: TDateEdit;
    dtp_end_date: TDateEdit;
    chb_dow1: TCheckBox;
    chb_dow2: TCheckBox;
    chb_dow3: TCheckBox;
    chb_dow4: TCheckBox;
    chb_dow5: TCheckBox;
    chb_dow6: TCheckBox;
    chb_dow7: TCheckBox;
    ed_repeat_interval: TEdit;
    Label5: TLabel;
    Label3: TLabel;
    btn_reset: TButton;
    Label6: TLabel;
    Label2: TLabel;
    gb_cmd: TGroupBox;
    lblParams: TLabel;
    lblcmd: TLabel;
    Label1: TLabel;
    ed_params: TEdit;
    chb_minimize: TCheckBox;
    btn_browse: TButton;
    chb_confirm: TCheckBox;
    btn_browse_params: TButton;
    ed_cmd: TEdit;
    btn_select_cmd: TButton;
    ed_dir: TEdit;
    chb_message: TCheckBox;
    ed_message: TEdit;
    chb_sound: TCheckBox;
    ed_sound: TEdit;
    chb_shutdown: TCheckBox;
    chb_force: TCheckBox;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    btn_stop: TButton;
    procedure btn_stopClick(Sender: TObject);
    procedure btn_resetClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btn_browseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btn_browse_paramsClick(Sender: TObject);
    procedure btn_startClick(Sender: TObject);
    procedure btn_cancelClick(Sender: TObject);
    procedure btn_select_cmdClick(Sender: TObject);
  protected
  private
    pause_timer: boolean;
    tasks: array [0..0] of _task;
    function GetTaskIndex: integer;
    procedure ResetTask(index: integer);
    function ItIsNow(value: TDateTime): boolean;
    procedure UpdateTaskNextTime(index: integer);
    procedure SaveTask(activate: boolean);
    procedure ShowTask(index: integer);
    procedure Exec(index: integer);
  public
    procedure TimerProc;
    procedure FromString(str: string; index: integer);
    function ToString(index: integer): string;
  end;


implementation
uses DockH;
{$R *.lfm}
//------------------------------------------------------------------------------
function PosEx(value, atext: string; startpos: integer): integer;
begin
  result:= startpos;
  if value = '' then exit;

  while result <= length(atext) do
  begin
    if atext[result] = value[1] then
      if copy(atext, result, length(value)) = value then exit;
    inc(result);
  end;
end;
//------------------------------------------------------------------------------
function cut(itext, ch: string): string;
var
  ipos: integer;
begin
  ipos:= pos(ch, itext);
  if ipos > 0 then result:= copy(itext, 1, ipos - 1) else result:= itext;
end;
//------------------------------------------------------------------------------
function cutafter(itext, ch: string): string;
var
  ipos: integer;
begin
  ipos:= pos(ch, itext);
  if ipos > 0 then result:= copy(itext, ipos + length(ch), length(itext)) else result:= '';
end;
//------------------------------------------------------------------------------
function FetchValue(itext: string; value, delim: string): string;
var
  ipos, ipos2: integer;
begin
  ipos:= system.pos(value, itext);
  if ipos > 0 then
  begin
    ipos2:= posex(delim, itext, ipos + length(value));
    result:= system.copy(itext, ipos + length(value), ipos2 - ipos - length(value));
  end else result:= '';
end;
//------------------------------------------------------------------------------
function TruncMSec(value: TDateTime): TDateTime;
begin
  result:= value - MilliSecondOf(value) / MSecsPerDay;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
procedure Tfrmscheduler.FormCreate(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
  ResetTask(0);
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.ResetTask(index: integer);
var
  h, m, s, ms: word;
begin
  decodetime(now, h, m, s, ms);

  tasks[index].active:= false;
  tasks[index].once:= true;
  tasks[index].timer:= false;
  tasks[index].daily:= false;
  tasks[index].once_date_time:= trunc(now) + h * 3600000 / MSecsPerDay + m * 60000 / MSecsPerDay;
  tasks[index].repeat_start:= tasks[index].once_date_time;
  tasks[index].repeat_end:= tasks[index].once_date_time;
  tasks[index].repeat_interval:= 30;
  tasks[index].repeat_dow:= $7f;
  tasks[index].use_repeat_start_date:= false;
  tasks[index].use_repeat_end_time:= false;
  tasks[index].use_repeat_end_date:= false;
  tasks[index].message:= true;
  tasks[index].message_text:= 'Событие планировщика';
  tasks[index].sound:= true;
  tasks[index].sound_file:= 'chimes.wav';
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:= cahide;
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.FormShow(Sender: TObject);
begin
  ShowTask(0);
end;
//------------------------------------------------------------------------------
function Tfrmscheduler.GetTaskIndex: integer;
begin
  result:= 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.btn_browseClick(Sender: TObject);
var
  c: array [0..MAX_PATH] of char;
begin
  DockletBrowseForImage(Handle, @c, nil);
  ed_cmd.text:= strpas(@c);
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.btn_browse_paramsClick(Sender: TObject);
var
  c: array [0..MAX_PATH] of char;
begin
  DockletBrowseForImage(Handle, @c, nil);
  ed_params.text:= strpas(@c);
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.btn_startClick(Sender: TObject);
begin
  SaveTask(true);
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.btn_stopClick(Sender: TObject);
begin
  SaveTask(false);
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.btn_cancelClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.btn_resetClick(Sender: TObject);
begin
  ResetTask(GetTaskIndex);
  ShowTask(GetTaskIndex);
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.ShowTask(index: integer);
begin
  btn_start.enabled:= not tasks[index].active;
  btn_stop.enabled:= tasks[index].active;

  if tasks[index].active then
    rb_daily.caption:= 'Повторять каждый день [следующее срабатывание ' + formatdatetime('hh:nn:ss  dd.MM.yyyy', tasks[index].next_time) + ']'
  else rb_daily.caption:= 'Повторять каждый день ';
  //
  rb_timer.checked:= tasks[index].timer;
  rb_once.checked:= tasks[index].once;
  rb_daily.checked:= tasks[index].daily;
  dtp_timer.time:= tasks[index].timer_time;

  dtp_once_date.Date:= tasks[index].once_date_time;
  dtp_once_time.Date:= tasks[index].once_date_time;
  dtp_start_date.Date:= tasks[index].repeat_start;
  dtp_start_time.Date:= tasks[index].repeat_start;
  dtp_end_date.Date:= tasks[index].repeat_end;
  dtp_end_time.Date:= tasks[index].repeat_end;

  dtp_start_date.checked:= tasks[index].use_repeat_start_date;
  dtp_end_time.checked:= tasks[index].use_repeat_end_time;
  dtp_end_date.checked:= tasks[index].use_repeat_end_date;

  ed_repeat_interval.text:= inttostr(tasks[index].repeat_interval);
  chb_dow1.checked:= boolean(tasks[index].repeat_dow and $1);
  chb_dow2.checked:= boolean(tasks[index].repeat_dow and $2);
  chb_dow3.checked:= boolean(tasks[index].repeat_dow and $4);
  chb_dow4.checked:= boolean(tasks[index].repeat_dow and $8);
  chb_dow5.checked:= boolean(tasks[index].repeat_dow and $10);
  chb_dow6.checked:= boolean(tasks[index].repeat_dow and $20);
  chb_dow7.checked:= boolean(tasks[index].repeat_dow and $40);
  //
  ed_cmd.text:= tasks[index].cmd;
  ed_params.text:= tasks[index].params;
  ed_dir.text:= tasks[index].dir;
  chb_confirm.checked:= tasks[index].confirm;
  chb_minimize.checked:= tasks[index].minimize;
  chb_message.checked:= tasks[index].message;
  chb_sound.checked:= tasks[index].sound;
  chb_shutdown.checked:= tasks[index].shutdown;
  chb_force.checked:= tasks[index].force;
  ed_message.text:= tasks[index].message_text;
  ed_sound.text:= tasks[index].sound_file;
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.SaveTask(activate: boolean);
var
  h, m, s, ms: word;
  ti: integer;
begin
  ti:= GetTaskIndex;
  tasks[ti].use_repeat_start_date:= dtp_start_date.checked;
  tasks[ti].use_repeat_end_time:= dtp_end_time.checked;
  tasks[ti].use_repeat_end_date:= dtp_end_date.checked;

  tasks[ti].active:= activate;

  tasks[ti].timer:= rb_timer.checked;
  tasks[ti].once:= rb_once.checked;
  tasks[ti].daily:= rb_daily.checked;
  tasks[ti].timer_time:= dtp_timer.time;
  ReplaceDate(tasks[ti].timer_time, 0);

  decodetime(dtp_once_time.Time, h, m, s, ms);
  tasks[ti].once_date_time:= dtp_once_date.DateTime;
  ReplaceTime(tasks[ti].once_date_time, encodetime(h, m, 0, 0));

  decodetime(dtp_start_time.Time, h, m, s, ms);
  tasks[ti].repeat_start:= dtp_start_date.DateTime;
  ReplaceTime(tasks[ti].repeat_start, encodetime(h, m, 0, 0));

  decodetime(dtp_end_time.Time, h, m, s, ms);
  tasks[ti].repeat_end:= dtp_end_date.DateTime;
  ReplaceTime(tasks[ti].repeat_end, encodetime(h, m, 0, 0));

  try tasks[ti].repeat_interval:= strtoint(ed_repeat_interval.text);
  except
    tasks[ti].repeat_interval:= 5;
    ed_repeat_interval.text:= '5';
  end;
  tasks[ti].repeat_dow:= integer(chb_dow1.checked) +
    integer(chb_dow2.checked) shl 1 +
    integer(chb_dow3.checked) shl 2 +
    integer(chb_dow4.checked) shl 3 +
    integer(chb_dow5.checked) shl 4 +
    integer(chb_dow6.checked) shl 5 +
    integer(chb_dow7.checked) shl 6;

  tasks[ti].cmd:= ed_cmd.text;
  tasks[ti].params:= ed_params.text;
  tasks[ti].dir:= ed_dir.text;
  tasks[ti].confirm:= chb_confirm.checked;
  tasks[ti].minimize:= chb_minimize.checked;
  tasks[ti].message:= chb_message.checked;
  tasks[ti].sound:= chb_sound.checked;
  tasks[ti].shutdown:= chb_shutdown.checked;
  tasks[ti].force:= chb_force.checked;
  tasks[ti].message_text:= ed_message.text;
  tasks[ti].sound_file:= ed_sound.text;

  UpdateTaskNextTime(ti);
  ShowTask(ti);
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.UpdateTaskNextTime(index: integer);
var
  h, m, s, ms: word;
  tmp: TDateTime;
begin
  // timer (countdown) //
  if tasks[index].timer then
  begin
    tasks[index].next_time:= TruncMSec(now);
    decodetime(tasks[index].timer_time, h, m, s, ms);
    TryFloatToDateTime(double(tasks[index].next_time) + h * 3600000 / MSecsPerDay + m * 60000 / MSecsPerDay + s * 1000 / MSecsPerDay, tasks[index].next_time);
  end;

  // execute once //
  if tasks[index].once then tasks[index].next_time:= TruncMSec(tasks[index].once_date_time);

  // daily execution //
  if tasks[index].daily then
  begin
    tasks[index].next_time:= trunc(now) + frac(tasks[index].repeat_start);

    if tasks[index].use_repeat_start_date and (trunc(tasks[index].next_time) < trunc(tasks[index].repeat_start)) then
      tasks[index].next_time:= tasks[index].repeat_start;

    if tasks[index].repeat_dow > 0 then
      while (tasks[index].repeat_dow and trunc(power(2, DayOfTheWeek(tasks[index].next_time) - 1))) = 0 do
        tasks[index].next_time:= tasks[index].next_time + 1;

    if tasks[index].use_repeat_end_time then
    begin
      while tasks[index].next_time <= now do
        tasks[index].next_time:= tasks[index].next_time + tasks[index].repeat_interval / MinsPerDay;
      if frac(tasks[index].next_time) > frac(tasks[index].repeat_end) then
        tasks[index].next_time:= trunc(now) + 1 + frac(tasks[index].repeat_start);
    end else begin
      while tasks[index].next_time <= now do tasks[index].next_time:= tasks[index].next_time + 1;
    end;

    if tasks[index].repeat_dow > 0 then
      while (tasks[index].repeat_dow and trunc(power(2, DayOfTheWeek(tmp) - 1))) = 0 do tasks[index].next_time:= tasks[index].next_time + 1;

    // check for task end //
    if tasks[index].use_repeat_end_date and (tasks[index].next_time > tasks[index].repeat_end) then
      tasks[index].active:= false;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.TimerProc;
var
  index: integer;
begin
  if pause_timer then exit;

  index:= 0;
  while index <= 0 do
  begin
    if tasks[index].active then
      if ItIsNow(tasks[index].next_time) then Exec(index);
    inc(index);
  end;
end;
//------------------------------------------------------------------------------
function Tfrmscheduler.ItIsNow(value: TDateTime): boolean;
var
  n: TDateTime;
begin
  n:= now;
  result:= (value < n) and (n <= value + 59 / SecsPerDay);
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.Exec(index: integer);
var
  showcmd: integer;
begin
  pause_timer:= true;

  UpdateTaskNextTime(index);
  // disable task if needed //
  if tasks[index].timer or tasks[index].once then tasks[index].active:= false;
  ShowTask(index);

  // exec actions //
  if tasks[index].message then dockh.HostExecute('/alert', pchar(tasks[index].message_text), nil, 0);
  if tasks[index].sound then dockh.HostExecute('/play', pchar(tasks[index].sound_file), nil, 0);
  if tasks[index].force then dockh.HostExecute('/exwin', '5', nil, 0)
  else if tasks[index].shutdown then dockh.HostExecute('/exwin', '1', nil, 0);

  // exec command //
  if trim(tasks[index].cmd) <> '' then
  try
    if tasks[index].minimize then showcmd:= sw_minimize else showcmd:= sw_shownormal;
    if tasks[index].confirm then
    begin
      if mrYes = messagebox(handle, pchar(tasks[index].cmd), 'Подтвердите выполнение действия', mb_yesno or mb_iconquestion) then
        dockh.HostExecute(pchar(tasks[index].cmd), pchar(tasks[index].params), pchar(tasks[index].dir), showcmd);
    end else begin
      dockh.HostExecute(pchar(tasks[index].cmd), pchar(tasks[index].params), pchar(tasks[index].dir), showcmd);
    end;
  except
    messagebox(handle, pchar(SysErrorMessage(GetLastError)), 'TerryClock', mb_iconerror);
  end;

  pause_timer:= false;
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.btn_select_cmdClick(Sender: TObject);
const
  CMD_COUNT = 12;
  desc: array [1..CMD_COUNT] of string = ('Выключить компьютер',
    'Перезагрузить компьютер', 'Выключить компьютер принудительно',
    'Ждущий режим', 'Вывести всплывающее сообщение (текст в параметрах)',
    'Проиграть стандарный звук (или другой в параметрах)',
    'Установить громкость в % (укажите % в параметрах)',
    'Вкл. проигрывание Winamp (Play)', 'Выкл. проигрывание Winamp (Stop)',
    'Завершить процесс (укажите имя в параметрах)', 'Скрыть Терри', 'Закрыть Терри');
  cmds: array [1..CMD_COUNT] of string = ('/exwin', '/exwin', '/exwin',
    '/suspend', '/alert', '/play', '/setvol', '/wa', '/wa',
    '/killprocess', '/hide', '/quit');
  params: array [1..CMD_COUNT] of string = ('1', '2', '5', '1,0,0',
    'Текст сообщения', '', '', 'play', 'stop', 'process.exe', '', '');
var
  hMenu: uint;
  cmd: integer;
  pt: windows.TPoint;
begin
	pt:= point(btn_select_cmd.Left, btn_select_cmd.BoundsRect.Bottom);
  pt:= gb_cmd.ClientToScreen(pt);
  hMenu:= CreatePopupMenu;

  for cmd:= 1 to CMD_COUNT do AppendMenu(hMenu, MF_STRING, cmd, pchar(desc[cmd]));

	cmd:= integer(TrackPopupMenu(hMenu,
    TPM_LEFTALIGN + TPM_TOPALIGN + TPM_RIGHTBUTTON + TPM_RETURNCMD,
    pt.x, pt.y, 0, handle, nil));
	DestroyMenu(hMenu);

  if (cmd > 0) and (cmd <= CMD_COUNT) then
  begin
    ed_cmd.text:= cmds[cmd];
    ed_params.text:= params[cmd];
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmscheduler.FromString(str: string; index: integer);
begin
  try tasks[index].active:= boolean(strtoint(FetchValue(str, 'active=', ';')));
  except end;

  try tasks[index].timer:= boolean(strtoint(FetchValue(str, 'timer=', ';')));
  except end;
  try tasks[index].once:= boolean(strtoint(FetchValue(str, 'once=', ';')));
  except end;
  try tasks[index].daily:= boolean(strtoint(FetchValue(str, 'daily=', ';')));
  except end;
  try tasks[index].timer_time:= strtotime(FetchValue(str, 'timer_time=', ';'));
  except end;

  try tasks[index].once_date_time:= strtodate(FetchValue(str, 'once_date=', ';'));
  except end;
  try ReplaceTime(tasks[index].once_date_time, strtotime(FetchValue(str, 'once_time=', ';')));
  except end;
  try tasks[index].repeat_start:= strtodate(FetchValue(str, 'repeat_start_date=', ';'));
  except end;
  try ReplaceTime(tasks[index].repeat_start, strtotime(FetchValue(str, 'repeat_start_time=', ';')));
  except end;
  try tasks[index].repeat_end:= strtodate(FetchValue(str, 'repeat_end_date=', ';'));
  except end;
  try ReplaceTime(tasks[index].repeat_end, strtotime(FetchValue(str, 'repeat_end_time=', ';')));
  except end;

  try tasks[index].repeat_interval:= strtoint(FetchValue(str, 'repeat_interval=', ';'));
  except end;
  try tasks[index].use_repeat_end_time:= boolean(strtoint(FetchValue(str, 'use_repeat_end_time=', ';')));
  except end;
  try tasks[index].use_repeat_end_date:= boolean(strtoint(FetchValue(str, 'use_repeat_end_date=', ';')));
  except end;
  try tasks[index].repeat_dow:= strtoint(FetchValue(str, 'repeat_dow=', ';'));
  except end;

  tasks[index].cmd:= FetchValue(str, 'cmd=', ';');
  tasks[index].params:= FetchValue(str, 'params=', ';');
  tasks[index].dir:= FetchValue(str, 'dir=', ';');
  try tasks[index].confirm:= boolean(strtoint(FetchValue(str, 'confirm=', ';')));
  except end;
  try tasks[index].minimize:= boolean(strtoint(FetchValue(str, 'minimize=', ';')));
  except end;
  try tasks[index].message:= boolean(strtoint(FetchValue(str, 'message=', ';')));
  except end;
  try tasks[index].sound:= boolean(strtoint(FetchValue(str, 'sound=', ';')));
  except end;
  try tasks[index].shutdown:= boolean(strtoint(FetchValue(str, 'shutdown=', ';')));
  except end;
  try tasks[index].force:= boolean(strtoint(FetchValue(str, 'force=', ';')));
  except end;
  tasks[index].message_text:= FetchValue(str, 'message_text=', ';');
  tasks[index].sound_file:= FetchValue(str, 'sound_file=', ';');

  if tasks[index].active then UpdateTaskNextTime(index);
end;
//------------------------------------------------------------------------------
function Tfrmscheduler.ToString(index: integer): string;
begin
  result:= '';
  if tasks[index].active then result:= result + 'active=1;';

  if tasks[index].timer then result:= result + 'timer=1;';
  if tasks[index].once then result:= result + 'once=1;';
  if tasks[index].daily then result:= result + 'daily=1;';
  result:= result + 'timer_time=' + timetostr(tasks[index].timer_time) + ';';

  result:= result + 'once_date=' + datetostr(tasks[index].once_date_time) + ';';
  result:= result + 'once_time=' + timetostr(tasks[index].once_date_time) + ';';
  result:= result + 'repeat_start_date=' + datetostr(tasks[index].repeat_start) + ';';
  result:= result + 'repeat_start_time=' + timetostr(tasks[index].repeat_start) + ';';
  result:= result + 'repeat_end_date=' + datetostr(tasks[index].repeat_end) + ';';
  result:= result + 'repeat_end_time=' + timetostr(tasks[index].repeat_end) + ';';

  result:= result + 'repeat_interval=' + inttostr(tasks[index].repeat_interval) + ';';
  if tasks[index].use_repeat_end_time then result:= result + 'use_repeat_end_time=1;';
  if tasks[index].use_repeat_end_date then result:= result + 'use_repeat_end_date=1;';
  result:= result + 'repeat_dow=' + inttostr(tasks[index].repeat_dow) + ';';

  if tasks[index].cmd <> '' then result:= result + 'cmd=' + tasks[index].cmd + ';';
  if tasks[index].params <> '' then result:= result + 'params=' + tasks[index].params + ';';
  if tasks[index].dir <> '' then result:= result + 'dir=' + tasks[index].dir + ';';
  if tasks[index].confirm then result:= result + 'confirm=1;';
  if tasks[index].minimize then result:= result + 'minimize=1;';
  if tasks[index].message then result:= result + 'message=1;';
  if tasks[index].sound then result:= result + 'sound=1;';
  if tasks[index].shutdown then result:= result + 'shutdown=1;';
  if tasks[index].force then result:= result + 'force=1;';
  if tasks[index].message_text <> '' then result:= result + 'message_text=' + tasks[index].message_text + ';';
  if tasks[index].sound_file <> '' then result:= result + 'sound_file=' + tasks[index].sound_file + ';';
end;
//------------------------------------------------------------------------------
end.
