package io.proleap.cobol.api.environment.inputoutput.filecontrol;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class FileControlEntry {

	protected File file;

	protected final String filename;

	protected FileReader fileReader;

	protected FileWriter fileWriter;

	protected PrintWriter printWriter;

	public FileControlEntry() {
		filename = "";
	}

	public FileControlEntry(final String filename) {
		this.filename = filename;
	}

	protected void assureFile() throws IOException {
		if (file == null) {
			file = File.createTempFile(filename, ".tmp");
		}
	}

	public void close() throws IOException {
		if (fileWriter != null) {
			fileWriter.close();
		}

		if (printWriter != null) {
			printWriter.close();
		}

		if (fileReader != null) {
			fileReader.close();
		}
	}

	public File getFile() {
		return file;
	}

	public void openRead() throws IOException {
		assureFile();

		fileReader = new FileReader(file);
	}

	public void openWrite(final boolean append) throws IOException {
		assureFile();

		fileWriter = new FileWriter(file, append);
		printWriter = new PrintWriter(fileWriter);
	}
}
