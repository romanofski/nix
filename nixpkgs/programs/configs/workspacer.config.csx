#r "C:\Program Files\workspacer\workspacer.Shared.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.Bar\workspacer.Bar.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.ActionMenu\workspacer.ActionMenu.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.FocusIndicator\workspacer.FocusIndicator.dll"
#r "C:\Windows\assembly\GAC_MSIL\Microsoft.Office.Interop.Outlook\15.0.0.0__71e9bce111e9429c\Microsoft.Office.Interop.Outlook.dll"
#r "C:\Windows\assembly\GAC_MSIL\office\15.0.0.0__71e9bce111e9429c\OFFICE.DLL"

using Microsoft.Office.Interop.Outlook;

using System;
using System.Diagnostics;
using System.Collections.Generic;
using SD = System.Drawing;
using System.Net;
using System.Linq;
using System.Text;
using System.IO;
using System.Xml;
using System.Timers;
using RE = System.Text.RegularExpressions;

using workspacer;
using workspacer.Bar;
using workspacer.Bar.Widgets;
using workspacer.ActionMenu;
using workspacer.FocusIndicator;

public record WeatherData(int Temperature);

class CalendarMonitor : BarWidgetBase
{
    private int _interval;
    private Timer _timer;
    private string _error;
    private List<AppointmentItem> _data;

    public CalendarMonitor(int interval)
    {
        _interval = interval;
    }

    public override IBarWidgetPart[] GetParts()
    {
        string msg;
        int ellipsisMaxLength = 40;

        if (_data.Count == 0)
        {
            msg = "0 mtgs";
            return Parts(Part(msg));
        }
        else
        {
            List<AppointmentItem> toDisplay = _data.Take(2).ToList();
            List<IBarWidgetPart> parts = new List<IBarWidgetPart>();
            foreach (AppointmentItem item in toDisplay)
            {
                Color color = Color.White;
                TimeSpan toMeeting = item.Start - DateTime.Now;
                if (toMeeting <= TimeSpan.FromMinutes(30))
                {
                    color = Color.Lime;
                }
                if (toMeeting <= TimeSpan.FromMinutes(15))
                {
                    color = Color.Red;  
                }
                string ellipsis = item.Subject.Length > ellipsisMaxLength ? string.Concat(item.Subject.AsSpan(0, ellipsisMaxLength), "...") : item.Subject;
                parts.Add(Part($"{item.Start:HH:mm} {ellipsis} - ", color, fontname: FontName));
            }
            return parts.ToArray();
        }
    }

    public override void Initialize()
    {
        _timer = new Timer(_interval);
        _timer.Elapsed += (s, e) => {
            try
            {
                // Create an Outlook application instance
                Application outlookApp = new Application();

                // Get the namespace (MAPI)
                NameSpace outlookNamespace = outlookApp.GetNamespace("MAPI");

                Items restrictedItems = GetCalendarItems(outlookNamespace);

                List<AppointmentItem> eventsToConsider = new List<AppointmentItem>();

                foreach(AppointmentItem item in restrictedItems)
                {
                    if (item.Subject.StartsWith("Canceled")) 
                    { 
                        continue; 
                    }
                    if (item.Start >= DateTime.Now)
                    {
                        eventsToConsider.Add(item);
                    }
                }
                _data = eventsToConsider;
            }
            catch (System.Exception ex)
            {
                _error = ex.Message;
            }
            MarkDirty();
        };
        _timer.Enabled = true;
    }

    static Items GetCalendarItems(NameSpace outlookNamespace, int hoursToQuery = 8)
    {
        // Get the default calendar folder
        MAPIFolder calendarFolder = outlookNamespace.GetDefaultFolder(OlDefaultFolders.olFolderCalendar);

        // Get the items in the calendar folder
        Items calendarItems = calendarFolder.Items;

        // Restrict the items to the next 8 hours
        DateTime startTime = DateTime.Now;
        DateTime endTime = startTime.AddHours(hoursToQuery);
        string filter = $"[Start] >= '{startTime:g}' AND [Start] <= '{endTime:g}'";


        Items restrictedItems = calendarItems.Restrict(filter);

        // Sort the items by start time
        restrictedItems.Sort("[Start]");
        restrictedItems.IncludeRecurrences = true;

        return restrictedItems;
    }
    public List<AppointmentItem> Main()
    {
            // Create an Outlook application instance
            Application outlookApp = new Application();

            // Get the namespace (MAPI)
            NameSpace outlookNamespace = outlookApp.GetNamespace("MAPI");

            Items restrictedItems = GetCalendarItems(outlookNamespace);

            List<AppointmentItem> eventsToConsider = new List<AppointmentItem>();

            foreach(AppointmentItem item in restrictedItems)
            {
                if (item.Subject.StartsWith("Canceled") || item.Subject.StartsWith("Declined")) 
                { 
                    continue; 
                }
                if (item.Start >= DateTime.Now)
                {
                    eventsToConsider.Add(item);
                }
            }

            return eventsToConsider;
    }
}

class ProcessMonitor : BarWidgetBase {
    private int _interval;
    private Timer _timer;
    private string _cmd;
    private string _args;
    private string _stdout;

    public ProcessMonitor(string cmd, string args = null, int interval = 1000 * 60)
    {
        _cmd = cmd;
        _args = args;
        _interval = interval;
    }

    public override void Initialize()
    {
        _timer = new Timer(_interval);
        _timer.Elapsed += (s, e) => {
            _stdout = ProcessStdout(_cmd, _args);
            MarkDirty();
        };
        _timer.Enabled = true;
    }

    public override IBarWidgetPart[] GetParts()
    {
        return Parts(Part(_stdout, fontname: FontName));
    }

    /* TODO: So many problems, but the first that it may block forever */
    public string ProcessStdout(string cmd, string args)
    {
        Process proc = new Process();
        proc.StartInfo.FileName = cmd;
        if (args != null)
        {
            proc.StartInfo.Arguments = args;
        }
        proc.StartInfo.UseShellExecute = false;
        proc.StartInfo.RedirectStandardOutput = true;
        proc.StartInfo.CreateNoWindow = true;
        proc.Start();
        proc.WaitForExit();
        return proc.StandardOutput.ReadToEnd();
    }
}

class WeatherWidget : BarWidgetBase {
    private int _interval;
    private Timer _timer;
    private string _stationID;
    private string _stationName;
    private WeatherData _data;

    public WeatherWidget(string stationName, string stationID, int interval)
    {
        _stationName = stationName;
        _stationID = stationID;
        _interval = interval;
    }

    public override IBarWidgetPart[] GetParts()
    {
        Color red = Color.Red;
        Color green = Color.Lime;
        Color currentColor = red;

        string formatted = String.Format("{0}: {1}°C", _stationName, _data.Temperature);

        if (_data.Temperature < 24)
        {
            currentColor = green;
        }
        return Parts(Part(formatted, currentColor, fontname: FontName));
    }

    public override void Initialize()
    {
        _timer = new Timer(_interval);
        _timer.Elapsed += (s, e) => {
            _data = WeatherData(_stationID);
            MarkDirty();
        };
        _timer.Enabled = true;
    }

    public WeatherData WeatherData(string stationID)
    {
        string contents = FetchText(stationID);
        return ParseWeatherTxt(contents);
    }

    private string FetchText(string stationID)
    {
        string uri = "https://tgftp.nws.noaa.gov/data/observations/metar/decoded/" + stationID + ".TXT";
        HttpWebRequest request = (HttpWebRequest) WebRequest.Create(uri);
        using (var response = (HttpWebResponse) request.GetResponse())
        {
            var encoding = Encoding.GetEncoding(response.CharacterSet);

            using (var responseStream = response.GetResponseStream())
            using (var reader = new StreamReader(responseStream, encoding))
                return reader.ReadToEnd();
        }
    }

    private WeatherData ParseWeatherTxt(string textContents)
    {
        string pattern = @"Temperature:.*\((?<temperature>\d+) C\)";
        RE.Regex rg = new RE.Regex(pattern);
        RE.MatchCollection matches = rg.Matches(textContents);

        // TODO guard against more matches
        RE.Match match = matches[0];
        RE.GroupCollection groups = match.Groups;
        return new WeatherData(Int16.Parse(groups["temperature"].Value));
    }

}

class UVWidget : BarWidgetBase {
    private int _interval;
    private Timer _timer;
    private string _locid;
    private float _uv;

    public UVWidget(string locid, int interval)
    {
        _locid = locid;
        _interval = interval;
    }

    public override IBarWidgetPart[] GetParts()
    {
        Color red = Color.Red;
        Color green = Color.Lime;
        Color currentUVColor = red; // it's always bad ;)
        string formatted = String.Format("UV: {0}", _uv);

        if (_uv < 2.5)
        {
            currentUVColor = green;
        }
        return Parts(Part(formatted, currentUVColor, fontname: FontName));
    }

    public override void Initialize()
    {
        _timer = new Timer(_interval);
        _timer.Elapsed += (s, e) => {
            _uv = UVData(_locid);
            MarkDirty();
        };
        _timer.Enabled = true;
    }

    public float UVData(string locid)
    {
        string contents = FetchUVXML();
        XmlDocument doc = GetUVXmlDocument(contents);
        string query = "/stations/location/name[contains(., '" + locid + "')]/../index";
        XmlNode node = doc.DocumentElement.SelectSingleNode(query);
        return float.Parse(node.InnerText);
    }

    private string FetchUVXML()
    {
        HttpWebRequest request = (HttpWebRequest) WebRequest.Create("https://uvdata.arpansa.gov.au/xml/uvvalues.xml");
        using (var response = (HttpWebResponse) request.GetResponse())
        {
            var encoding = Encoding.GetEncoding(response.CharacterSet);

            using (var responseStream = response.GetResponseStream())
            using (var reader = new StreamReader(responseStream, encoding))
                return reader.ReadToEnd();
        }
    }

    private XmlDocument GetUVXmlDocument(string contents)
    {
        XmlDocument doc = new XmlDocument();
        doc.LoadXml(contents);
        return doc;
    }
}


Action<IConfigContext> doConfig = (context) =>
{
    // Uncomment to switch update branch (or to disable updates)
    //context.Branch = Branch.None;

    SD.Color bgC = SD.ColorTranslator.FromHtml("#073642");
    SD.Color fgC = SD.ColorTranslator.FromHtml("#9e9090");
    Color backgroundColor = new Color(bgC.R, bgC.G, bgC.B);
    Color foregroundColor = new Color(fgC.R, fgC.G, fgC.B);

    int oneMinute = 1000 * 60;

    context.ConsoleLogLevel = LogLevel.Debug;
    context.FileLogLevel = LogLevel.Debug;

    context.AddBar(new BarPluginConfig() {
        BarTitle = "workspacer.Bar",
        Background = backgroundColor,
        DefaultWidgetBackground = backgroundColor,
        DefaultWidgetForeground = foregroundColor,
        BarHeight = 18,
        FontSize = 10,
        FontName = "IBM Plex Mono",

        RightWidgets = () => new IBarWidget[] {
            new TextWidget("|>"),
            new CalendarMonitor(oneMinute * 5),
            new TextWidget(" | "),
            new ProcessMonitor("wsl.exe", "-d NixOS workbalance", oneMinute),
            new TextWidget(" | "),
            new UVWidget("bri", 1000),
            new TextWidget(" | "),
            new TimeWidget(1000, " ddd dd.MMM.yyyy HH:mm "),
            new TextWidget(" | "),
            new WeatherWidget("Brisbane", "YBBN", 1000),
            },
    }); ; ;
    context.AddFocusIndicator();

    context.WorkspaceContainer.CreateWorkspaces("1", "2", "3", "4", "5");
    context.CanMinimizeWindows = false; // false by default

    context.Keybinds.Subscribe(KeyModifiers.Alt, Keys.Tab, () => context.Workspaces.FocusedWorkspace.FocusNextWindow(), "cycle through windows on current workspace");

    context.WindowRouter.AddFilter((window) => !window.ProcessName.Contains("1Password"));
    context.WindowRouter.AddFilter((window) => !window.ProcessName.Contains("mstsc"));
    context.WindowRouter.AddFilter((window) => !window.Title.Contains("Windows Security"));
    context.WindowRouter.AddRoute((window) => window.ProcessName.Contains("firefox") ? context.WorkspaceContainer["2"] : null);
    context.WindowRouter.AddRoute((window) => window.ProcessName.Contains("edge") ? context.WorkspaceContainer["2"] : null);
};
return doConfig;
