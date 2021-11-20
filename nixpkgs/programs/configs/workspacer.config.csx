#r "C:\Program Files\workspacer\workspacer.Shared.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.Bar\workspacer.Bar.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.ActionMenu\workspacer.ActionMenu.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.FocusIndicator\workspacer.FocusIndicator.dll"

using System;
using System.Diagnostics;
using SD = System.Drawing;
using System.Net;
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

class ProcessMonitor : BarWidgetBase {
    private int _interval;
    private Timer _timer;
    private string _cmd;
    private string _args;
    private string _stdout;

    public ProcessMonitor(string cmd, string args, int interval)
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
        proc.StartInfo.Arguments = args;
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

    context.AddBar( new BarPluginConfig() {
        BarTitle = "workspacer.Bar",
        Background = backgroundColor,
        DefaultWidgetBackground = backgroundColor,
        DefaultWidgetForeground = foregroundColor,
        BarHeight = 18,
        FontSize = 10,
        FontName = "IBM Plex Mono",

        RightWidgets = () => new IBarWidget[] {
            new ProcessMonitor("wsl.exe", "-d NixOS workbalance", oneMinute),
            new TextWidget(" | "),
            new UVWidget("bri", 1000),
            new TextWidget(" | "),
            new TimeWidget(1000, " ddd dd.MMM.yyyy HH:mm "),
            new TextWidget(" | "),
            new WeatherWidget("Brisbane", "YBBN", 1000),
            },
    });
    context.AddFocusIndicator();

    context.WorkspaceContainer.CreateWorkspaces("1", "2", "3", "4", "5");
    context.CanMinimizeWindows = true; // false by default

    context.Keybinds.Subscribe(KeyModifiers.Alt, Keys.Tab, () => context.Workspaces.FocusedWorkspace.FocusNextWindow(), "cycle through windows on current workspace");
};
return doConfig;
