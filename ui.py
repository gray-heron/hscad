import gi
import subprocess
import os

gi.require_version("Gtk", "3.0")
from gi.repository import Gtk
from gi.repository import GLib

class Ui(Gtk.Window):
    def __init__(self):
        super().__init__(title="HsCAD")

        self.checkboxes_vis = {}
        self.checkboxes_solids = {}
        self.lastModelChange = None

        self.set_border_width(10)
        self.vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)
        self.vbox_vis = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)
        self.vbox_solid = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)
        self.vbox.add(Gtk.Label(label="Solid groups:", xalign=0))
        self.vbox.add(self.vbox_solid)
        self.vbox.add(Gtk.Label(label="", xalign=0))
        self.vbox.add(Gtk.Label(label="Visualisation groups:", xalign=0))
        self.vbox.add(self.vbox_vis)
        self.vbox.add(Gtk.Label(label=" ", xalign=0))
        self.add(self.vbox)

        hbox = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=50)
        self.vbox.add(hbox)

        label = Gtk.Label(label="Release", xalign=0)
        hbox.pack_start(label, True, True, 0)

        switch = Gtk.Switch()
        switch.props.valign = Gtk.Align.CENTER
        hbox.pack_start(switch, False, True, 0)

        self.monitorModelChanged()
        GLib.timeout_add(100, self.monitorModelChanged)


    def monitorModelChanged(self):
        modelChange = os.stat("Main.hs")
        if self.lastModelChange is not None and self.lastModelChange == modelChange:
            return True

        self.lastModelChange = modelChange

        solidGroups,visGroups = self.getModelMetaData()
        somethingChanged = False
        for g in solidGroups:
            if not g in self.checkboxes_solids:
                self.checkboxes_solids[g] = Gtk.CheckButton(label=g)
                self.checkboxes_solids[g].set_active(True)
                self.checkboxes_solids[g].connect("clicked", self.on_button_toggled, g)
                self.vbox_solid.add(self.checkboxes_solids[g])
                somethingChanged = True

        for v in visGroups:
            if not v in self.checkboxes_vis:
                self.checkboxes_vis[v] = Gtk.CheckButton(label=v)
                self.checkboxes_vis[v].set_active(True)
                self.checkboxes_vis[v].connect("clicked", self.on_button_toggled, v)
                self.vbox_vis.add(self.checkboxes_vis[v])
                somethingChanged = True

        self.show_all()

        if somethingChanged:
            self.dumpSettings()

        return True

    def getModelMetaData(self):
        output = subprocess.check_output("stack exec -- runghc Main.hs --info", shell=True).splitlines()
        return (eval(output[0]), eval(output[1])) 

    def on_button_toggled(self, button, name):
        self.dumpSettings()

    def dumpSettings(self):
        solids = " ".join([cb for cb in self.checkboxes_solids if self.checkboxes_solids[cb].get_active()])
        vis = " ".join([cb for cb in self.checkboxes_vis if self.checkboxes_vis[cb].get_active()])
        open("settings", "w").write(solids + "\n" + vis)
        subprocess.check_output("./produce_model.bash", shell=True)


win = Ui()
win.connect("destroy", Gtk.main_quit)
win.show_all()
Gtk.main()