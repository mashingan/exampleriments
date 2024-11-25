import { EventEmitter } from 'events';

// Define a custom event interface
interface CustomEvent {
  eventName: string;
  eventData: any;
}

// Define a class that extends EventEmitter
class CustomEventEmitter extends EventEmitter {
  // Method to emit custom events
  emitCustomEvent(event: CustomEvent) {
    this.emit(event.eventName, event.eventData);
  }

  // Method to listen for custom events
  onCustomEvent(eventName: string, listener: (data: any) => void) {
    this.on(eventName, listener);
  }
}

// Example usage
const customEmitter = new CustomEventEmitter();

// Listener for custom event
customEmitter.onCustomEvent('customEvent', (data: any) => {
  console.log('Custom event received:', data);
});

// Emitting a custom event
const eventData = { message: 'Hello, world!' };
const customEvent: CustomEvent = { eventName: 'customEvent', eventData };
customEmitter.emitCustomEvent(customEvent);
